(* Tests for sml-lisp, standardized on the shared sml-test Harness. *)

structure Tests =
struct
  open Harness

structure L = Lisp

(* run a program and compare its printed result *)
fun evalsTo (src, expected) =
    (L.run src = expected) handle L.LispError _ => false

fun raisesError src =
    (ignore (L.run src); false) handle L.LispError _ => true

fun run () =
  let
    (* reader / printer round-trips *)
    val () = check "read int" (L.print (L.read "42") = "42")
    val () = check "read negative ~" (L.print (L.read "~7") = "~7")
    val () = check "read negative -" (L.print (L.read "-7") = "~7")
    val () = check "read symbol" (L.print (L.read "foo") = "foo")
    val () = check "read list" (L.print (L.read "(1 2 3)") = "(1 2 3)")
    val () = check "read nested" (L.print (L.read "(a (b c) d)") = "(a (b c) d)")
    val () = check "read quote sugar" (L.print (L.read "'x") = "(quote x)")
    val () = check "read #t/#f" (L.print (L.read "#t") = "#t")

    (* arithmetic with bignums *)
    val () = check "add" (evalsTo ("(+ 1 2 3)", "6"))
    val () = check "sub" (evalsTo ("(- 10 3 2)", "5"))
    val () = check "mul" (evalsTo ("(* 2 3 4)", "24"))
    val () = check "div" (evalsTo ("(div 20 4)", "5"))
    val () = check "nested arith" (evalsTo ("(+ (* 2 3) (- 10 4))", "12"))
    val () = check "bignum 2^100"
                   (evalsTo ("(letrec ((p (lambda (b e) (if (= e 0) 1 (* b (p b (- e 1))))))) (p 2 100))",
                             "1267650600228229401496703205376"))

    (* comparisons / booleans *)
    val () = check "less true" (evalsTo ("(< 1 2 3)", "#t"))
    val () = check "less false" (evalsTo ("(< 1 3 2)", "#f"))
    val () = check "eq numbers" (evalsTo ("(= 5 5)", "#t"))
    val () = check "not" (evalsTo ("(not #f)", "#t"))

    (* if / cond *)
    val () = check "if true" (evalsTo ("(if #t 1 2)", "1"))
    val () = check "if false" (evalsTo ("(if #f 1 2)", "2"))
    val () = check "if non-bool truthy" (evalsTo ("(if 0 1 2)", "1"))
    val () = check "cond first match" (evalsTo ("(cond (#f 1) (#t 2) (else 3))", "2"))
    val () = check "cond else" (evalsTo ("(cond (#f 1) (#f 2) (else 3))", "3"))

    (* and / or short-circuit *)
    val () = check "and all true returns last" (evalsTo ("(and 1 2 3)", "3"))
    val () = check "and short circuits" (evalsTo ("(and #f (car 5))", "#f"))
    val () = check "or returns first truthy" (evalsTo ("(or #f 7 8)", "7"))
    val () = check "or all false" (evalsTo ("(or #f #f)", "#f"))

    (* quote *)
    val () = check "quote list" (evalsTo ("'(1 2 3)", "(1 2 3)"))
    val () = check "quote symbol" (evalsTo ("'hello", "hello"))

    (* lists *)
    val () = check "cons/car/cdr"
                   (evalsTo ("(car (cdr (cons 1 (cons 2 (quote ())))))", "2"))
    val () = check "list builtin" (evalsTo ("(list 1 2 3)", "(1 2 3)"))
    val () = check "null? empty" (evalsTo ("(null? (quote ()))", "#t"))
    val () = check "null? nonempty" (evalsTo ("(null? (list 1))", "#f"))

    (* define + closures capturing environment *)
    val () = check "define and use"
                   (evalsTo ("(define x 10) (define y 32) (+ x y)", "42"))
    val () = check "define function sugar"
                   (evalsTo ("(define (sq n) (* n n)) (sq 9)", "81"))
    val () = check "closure captures env"
                   (evalsTo ("(define (adder n) (lambda (x) (+ x n))) (define add5 (adder 5)) (add5 100)",
                             "105"))
    val () = check "let binds locally"
                   (evalsTo ("(let ((a 3) (b 4)) (+ (* a a) (* b b)))", "25"))

    (* factorial = 120 *)
    val () = check "factorial 5 = 120"
                   (evalsTo ("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)",
                             "120"))

    (* letrec mutual recursion: even?/odd? *)
    val mutual =
        "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) " ^
        "         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1)))))) " ^
        "  (list (even? 10) (odd? 10) (even? 7)))"
    val () = check "letrec mutual recursion even?/odd?"
                   (evalsTo (mutual, "(#t #f #f)"))

    (* PROPER TAIL CALLS: a 100k-deep tail-recursive loop must not overflow *)
    val tailLoop =
        "(define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc 1)))) " ^
        "(loop 100000 0)"
    val () = check "100k tail-recursive loop (no stack overflow)"
                   (evalsTo (tailLoop, "100000"))

    (* tail position inside cond/begin/and/or also loops in constant stack *)
    val tailCond =
        "(define (count i) (cond ((= i 0) (quote done)) (else (count (- i 1))))) " ^
        "(count 200000)"
    val () = check "200k tail loop via cond"
                   (evalsTo (tailCond, "done"))

    (* error cases *)
    val () = check "unbound variable errors" (raisesError "nope")
    val () = check "arity mismatch errors"
                   (raisesError "(define (f a b) (+ a b)) (f 1)")
    val () = check "calling non-function errors" (raisesError "(5 6)")
    val () = check "car of non-pair errors" (raisesError "(car 3)")

    (* empty program *)
    val () = check "empty program -> empty string" (L.run "   " = "")
    val () = check "comments are ignored"
                   (evalsTo ("; a comment\n(+ 1 2) ; trailing\n", "3"))
  in
    Harness.run ()
  end
end
