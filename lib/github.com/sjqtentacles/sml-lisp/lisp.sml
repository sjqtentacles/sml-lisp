(* lisp.sml

   Implementation of the LISP signature: a reader (s-expression parser) and a
   lexically scoped evaluator with proper tail calls.

   Tail calls. `eval` is written as an explicit loop over a mutable pair
   `(expr, env)`: any construct whose result is the value of a sub-expression in
   tail position (`if`, `cond`, `begin`, `and`, `or`, the body of a `lambda`
   application, `let`/`letrec` bodies) updates the loop variables and continues
   rather than calling `eval` recursively. This guarantees constant stack usage
   for tail recursion uniformly on MLton and Poly/ML. *)

structure Lisp :> LISP =
struct
  datatype value =
      Int of IntInf.int
    | Bool of bool
    | Str of string
    | Sym of string
    | Nil
    | Cons of value * value
    | Closure of string list * value list * env
    | Prim of string * (value list -> value)

  and env = Env of (string * value ref) list ref list

  exception LispError of string

  (* ---------------------------------------------------------------- reader *)

  (* Hand-written recursive-descent reader over a char list with an index.
     (Parsec-style structure, but self-contained so the package has no deps.) *)

  fun isDelim c =
      Char.isSpace c orelse c = #"(" orelse c = #")"
      orelse c = #"\"" orelse c = #";"

  (* skip whitespace and ; line comments; return new index *)
  fun skipWs (s, i) =
      if i >= String.size s then i
      else let val c = String.sub (s, i)
           in if Char.isSpace c then skipWs (s, i + 1)
              else if c = #";"
                   then let fun toEol j =
                              if j >= String.size s then j
                              else if String.sub (s, j) = #"\n" then j + 1
                              else toEol (j + 1)
                        in skipWs (s, toEol (i + 1)) end
              else i
           end

  (* parse one datum starting at index i (after caller's skipWs);
     returns (value, nextIndex) *)
  fun parseDatum (s, i) =
      if i >= String.size s then raise LispError "unexpected end of input"
      else
        let val c = String.sub (s, i) in
          if c = #"(" then parseList (s, i + 1, [])
          else if c = #")" then raise LispError "unexpected )"
          else if c = #"'" then
            let val (v, j) = parseDatum (s, skipWs (s, i + 1))
            in (Cons (Sym "quote", Cons (v, Nil)), j) end
          else if c = #"\"" then parseString (s, i + 1, [])
          else parseAtom (s, i)
        end

  and parseList (s, i, acc) =
      let val i = skipWs (s, i) in
        if i >= String.size s then raise LispError "unterminated list"
        else if String.sub (s, i) = #")"
        then (List.foldr (fn (x, r) => Cons (x, r)) Nil (List.rev acc), i + 1)
        else let val (v, j) = parseDatum (s, i)
             in parseList (s, j, v :: acc) end
      end

  and parseString (s, i, acc) =
      if i >= String.size s then raise LispError "unterminated string"
      else let val c = String.sub (s, i) in
        if c = #"\"" then (Str (implode (List.rev acc)), i + 1)
        else if c = #"\\" andalso i + 1 < String.size s then
          let val e = String.sub (s, i + 1)
              val ch = case e of #"n" => #"\n" | #"t" => #"\t"
                              | #"\\" => #"\\" | #"\"" => #"\"" | other => other
          in parseString (s, i + 2, ch :: acc) end
        else parseString (s, i + 1, c :: acc)
      end

  and parseAtom (s, i) =
      let fun take j = if j >= String.size s orelse isDelim (String.sub (s, j))
                       then j else take (j + 1)
          val j = take i
          val tok = String.substring (s, i, j - i)
      in (atomOf tok, j) end

  and atomOf tok =
      if tok = "#t" then Bool true
      else if tok = "#f" then Bool false
      else case IntInf.fromString tok of
               (* IntInf.fromString accepts a leading ~; also accept '-' *)
               SOME n =>
                 (* reject things like "1a" that fromString would truncate *)
                 if validInt tok then Int n else Sym tok
             | NONE =>
                 (case parseDash tok of SOME n => Int n | NONE => Sym tok)

  and validInt tok =
      let val body = if String.size tok > 0 andalso
                        (String.sub (tok,0) = #"~" orelse String.sub (tok,0) = #"-")
                     then String.extract (tok, 1, NONE) else tok
      in body <> "" andalso CharVector.all Char.isDigit body end

  and parseDash tok =
      if String.size tok > 1 andalso String.sub (tok, 0) = #"-"
         andalso CharVector.all Char.isDigit (String.extract (tok, 1, NONE))
      then SOME (IntInf.~ (valOf (IntInf.fromString (String.extract (tok, 1, NONE)))))
      else NONE

  fun read s =
      let val i = skipWs (s, 0)
          val (v, j) = parseDatum (s, i)
          val k = skipWs (s, j)
      in if k >= String.size s then v
         else raise LispError "trailing input after datum" end

  fun readAll s =
      let fun loop (i, acc) =
              let val i = skipWs (s, i) in
                if i >= String.size s then List.rev acc
                else let val (v, j) = parseDatum (s, i)
                     in loop (j, v :: acc) end
              end
      in loop (0, []) end

  (* ---------------------------------------------------------------- printer *)

  fun print v =
      case v of
          Int n => IntInf.toString n
        | Bool true => "#t"
        | Bool false => "#f"
        | Str s => "\"" ^ s ^ "\""
        | Sym s => s
        | Nil => "()"
        | Cons _ => "(" ^ printList v ^ ")"
        | Closure _ => "#<closure>"
        | Prim (name, _) => "#<primitive:" ^ name ^ ">"

  and printList v =
      case v of
          Cons (h, Nil) => print h
        | Cons (h, (t as Cons _)) => print h ^ " " ^ printList t
        | Cons (h, t) => print h ^ " . " ^ print t
        | _ => print v

  (* ---------------------------------------------------------------- env *)

  fun lookup (Env [] : env) name = raise LispError ("unbound variable: " ^ name)
    | lookup (Env (frame :: rest)) name =
        (case List.find (fn (k, _) => k = name) (! frame) of
            SOME (_, r) => r
          | NONE => lookup (Env rest) name)

  fun define (Env env : env) name v =
      case env of
          [] => raise LispError "no environment frame"
        | frame :: _ =>
            (case List.find (fn (k, _) => k = name) (! frame) of
                 SOME (_, r) => r := v
               | NONE => frame := (name, ref v) :: (! frame))

  fun pushFrame (Env env : env) binds : env =
      Env (ref (map (fn (k, v) => (k, ref v)) binds) :: env)

  (* ---------------------------------------------------------------- helpers *)

  fun toList Nil = []
    | toList (Cons (h, t)) = h :: toList t
    | toList _ = raise LispError "improper list where list expected"

  fun isTruthy (Bool false) = false
    | isTruthy _ = true

  fun symName (Sym s) = s
    | symName _ = raise LispError "expected symbol"

  (* ---------------------------------------------------------------- eval *)

  fun eval (expr, env) =
      let
        (* explicit loop; tail positions update (e, n) and continue *)
        fun loop (e, n : env) =
            case e of
                Int _ => e
              | Bool _ => e
              | Str _ => e
              | Nil => e
              | Closure _ => e
              | Prim _ => e
              | Sym s => ! (lookup n s)
              | Cons (Sym "quote", Cons (x, Nil)) => x
              | Cons (Sym "if", Cons (c, Cons (th, els))) =>
                  if isTruthy (loop (c, n))
                  then loop (th, n)
                  else (case els of Cons (e2, Nil) => loop (e2, n)
                                  | Nil => Bool false
                                  | _ => raise LispError "malformed if")
              | Cons (Sym "cond", clauses) => evalCond (toList clauses, n)
              | Cons (Sym "and", args) => evalAnd (toList args, n)
              | Cons (Sym "or", args) => evalOr (toList args, n)
              | Cons (Sym "begin", body) => evalSeq (toList body, n)
              | Cons (Sym "quote", _) => raise LispError "malformed quote"
              | Cons (Sym "lambda", Cons (params, body)) =>
                  Closure (map symName (toList params), toList body, n)
              | Cons (Sym "define", Cons (Sym name, Cons (rhs, Nil))) =>
                  (define n name (loop (rhs, n)); Sym name)
              | Cons (Sym "define", Cons (Cons (Sym name, params), body)) =>
                  (* (define (f a b) body...) sugar *)
                  (define n name
                     (Closure (map symName (toList params), toList body, n));
                   Sym name)
              | Cons (Sym "let", Cons (binds, body)) =>
                  let val bs = map (fn Cons (Sym k, Cons (v, Nil)) =>
                                       (k, loop (v, n))
                                     | _ => raise LispError "malformed let binding")
                                   (toList binds)
                      val n' = pushFrame n bs
                  in evalSeqIn (toList body, n') end
              | Cons (Sym "letrec", Cons (binds, body)) =>
                  let val parsed = map (fn Cons (Sym k, Cons (v, Nil)) => (k, v)
                                         | _ => raise LispError "malformed letrec binding")
                                       (toList binds)
                      val n' = pushFrame n (map (fn (k, _) => (k, Bool false)) parsed)
                      val () = app (fn (k, rhs) => define n' k (loop (rhs, n'))) parsed
                  in evalSeqIn (toList body, n') end
              | Cons (f, args) =>
                  let val fv = loop (f, n)
                      val argvs = map (fn a => loop (a, n)) (toList args)
                  in case fv of
                         Prim (_, g) => g argvs
                       | Closure (params, body, cenv) =>
                           if length params <> length argvs
                           then raise LispError "arity mismatch"
                           else
                             let val n' = pushFrame cenv (ListPair.zip (params, argvs))
                             in (* tail-call the body in the new frame *)
                                case body of
                                    [] => raise LispError "empty lambda body"
                                  | _ => loopSeq (body, n')
                             end
                       | _ => raise LispError "attempt to call a non-function"
                  end

        (* evaluate a sequence, all but last for effect; last in tail position *)
        and loopSeq ([x], n) = loop (x, n)
          | loopSeq (x :: rest, n) = (loop (x, n); loopSeq (rest, n))
          | loopSeq ([], _) = Bool false

        and evalSeq (xs, n) = loopSeq (xs, n)
        and evalSeqIn (xs, n) = loopSeq (xs, n)

        and evalCond ([], _) = Bool false
          | evalCond (clause :: rest, n) =
              (case clause of
                   Cons (Sym "else", body) => loopSeq (toList body, n)
                 | Cons (test, body) =>
                     if isTruthy (loop (test, n))
                     then (case toList body of [] => Bool true
                                             | b => loopSeq (b, n))
                     else evalCond (rest, n)
                 | _ => raise LispError "malformed cond clause")

        and evalAnd ([], _) = Bool true
          | evalAnd ([x], n) = loop (x, n)
          | evalAnd (x :: rest, n) =
              if isTruthy (loop (x, n)) then evalAnd (rest, n) else Bool false

        and evalOr ([], _) = Bool false
          | evalOr ([x], n) = loop (x, n)
          | evalOr (x :: rest, n) =
              let val v = loop (x, n)
              in if isTruthy v then v else evalOr (rest, n) end
      in
        loop (expr, env)
      end

  (* ---------------------------------------------------------------- prims *)

  fun wantInt (Int n) = n
    | wantInt _ = raise LispError "expected integer"

  fun foldInts f init args = List.foldl (fn (v, acc) => f (acc, wantInt v)) init args

  fun arith name f identityOpt args =
      case args of
          [] => (case identityOpt of SOME i => Int i
                                   | NONE => raise LispError (name ^ ": needs args"))
        | [x] => (case identityOpt of
                      (* unary minus / div behaviour *)
                      SOME i => Int (f (i, wantInt x))
                    | NONE => Int (wantInt x))
        | x :: rest => Int (List.foldl (fn (v, acc) => f (acc, wantInt v))
                                       (wantInt x) rest)

  fun cmpChain f args =
      let fun go (a :: (rest as b :: _)) =
                f (wantInt a, wantInt b) andalso go rest
            | go _ = true
      in Bool (go args) end

  fun baseEnv () : env =
      let
        fun p name g = (name, ref (Prim (name, g)))
        val frame =
          [ p "+" (fn args => Int (foldInts (op +) 0 args)),
            p "*" (fn args => Int (foldInts (op * ) 1 args)),
            p "-" (arith "-" (op -) NONE),
            p "div" (arith "div" (fn (a,b) => IntInf.div (a,b)) NONE),
            p "mod" (fn [a,b] => Int (IntInf.mod (wantInt a, wantInt b))
                      | _ => raise LispError "mod: 2 args"),
            p "=" (cmpChain (op =)),
            p "<" (cmpChain (op <)),
            p ">" (cmpChain (op >)),
            p "<=" (cmpChain (op <=)),
            p ">=" (cmpChain (op >=)),
            p "cons" (fn [a,b] => Cons (a,b) | _ => raise LispError "cons: 2 args"),
            p "car" (fn [Cons (a,_)] => a | _ => raise LispError "car: pair"),
            p "cdr" (fn [Cons (_,d)] => d | _ => raise LispError "cdr: pair"),
            p "null?" (fn [Nil] => Bool true | [_] => Bool false
                        | _ => raise LispError "null?: 1 arg"),
            p "pair?" (fn [Cons _] => Bool true | [_] => Bool false
                        | _ => raise LispError "pair?: 1 arg"),
            p "list" (fn args => List.foldr Cons Nil args),
            p "not" (fn [v] => Bool (not (isTruthy v)) | _ => raise LispError "not: 1 arg"),
            p "eq?" (fn [a,b] => Bool (eqVal (a,b)) | _ => raise LispError "eq?: 2 args") ]
      in Env [ref frame] end

  and eqVal (Int a, Int b) = a = b
    | eqVal (Bool a, Bool b) = a = b
    | eqVal (Str a, Str b) = a = b
    | eqVal (Sym a, Sym b) = a = b
    | eqVal (Nil, Nil) = true
    | eqVal _ = false

  (* ---------------------------------------------------------------- run *)

  fun run src =
      let val prog = readAll src
          val env = baseEnv ()
          fun loop ([], last) = last
            | loop (e :: rest, _) = loop (rest, SOME (eval (e, env)))
      in case loop (prog, NONE) of
             NONE => ""
           | SOME v => print v
      end
end
