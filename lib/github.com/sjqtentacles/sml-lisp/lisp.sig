(* lisp.sig

   A small, functional Scheme interpreter for Standard ML.

   Values use arbitrary-precision integers (`IntInf.int`). The evaluator is
   lexically scoped, supports first-class closures, and evaluates tail calls in
   constant stack space (so loops written as tail recursion do not overflow).

   `read` parses one datum from a string; `readAll` parses a sequence. `eval`
   evaluates a datum in an environment. `run` reads a whole program (a sequence
   of top-level forms), evaluates them left to right in a fresh global
   environment seeded with the primitives, and returns the printed form of the
   last result. *)

signature LISP =
sig
  datatype value =
      Int of IntInf.int
    | Bool of bool
    | Str of string
    | Sym of string
    | Nil                       (* the empty list '() *)
    | Cons of value * value
    | Closure of string list * value list * env  (* params, body, captured env *)
    | Prim of string * (value list -> value)

  (* Environments are mutually recursive with values (closures capture an env).
     The representation is hidden under opaque ascription. *)
  and env = Env of (string * value ref) list ref list

  exception LispError of string

  (* Parse exactly one datum (ignoring surrounding whitespace/comments).
     Raises LispError on malformed or trailing input. *)
  val read : string -> value

  (* Parse a sequence of data (a whole program). *)
  val readAll : string -> value list

  (* Render a value back to its textual s-expression form. *)
  val print : value -> string

  (* A fresh global environment with the standard primitives bound. *)
  val baseEnv : unit -> env

  (* Evaluate a datum in an environment. Tail calls use constant stack. *)
  val eval : value * env -> value

  (* Read + evaluate a whole program; return the printed last result
     ("" if the program is empty). *)
  val run : string -> string
end
