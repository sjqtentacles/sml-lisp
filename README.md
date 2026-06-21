# sml-lisp

[![CI](https://github.com/sjqtentacles/sml-lisp/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-lisp/actions/workflows/ci.yml)

A small, functional Scheme interpreter written in Standard ML.

`sml-lisp` is a compact Lisp/Scheme evaluator: a hand-written s-expression
reader plus a lexically scoped evaluator with first-class closures,
arbitrary-precision integers, and **proper tail calls** (tail-recursive loops
run in constant stack space, so they don't overflow).

## Features

- Arbitrary-precision integers via `IntInf` (e.g. `2^100` is exact).
- Lexical scoping and first-class closures that capture their environment.
- Special forms: `quote` (and `'` sugar), `if`, `cond`, `and`, `or`,
  `lambda`, `define` (value and `(define (f x) ...)` function sugar), `let`,
  `letrec`, `begin`.
- Primitives: `+ - * div mod = < > <= >= cons car cdr null? pair? list not eq?`.
- Proper tail calls: `if`, `cond`, `begin`, `and`, `or`, and lambda bodies all
  evaluate their tail sub-expression in constant stack.
- `;` line comments and `"..."` string literals with escapes.

## Portability

Pure Standard ML using only the Basis library. Verified on:

- **MLton**
- **Poly/ML**

Tail-call behavior is exercised by the test suite (100k- and 200k-deep
tail-recursive loops complete without stack overflow on both compilers).

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Installing with smlpkg

`sml-lisp` follows the conventions of the
[`smlpkg`](https://github.com/diku-dk/smlpkg) package manager. Packages are
referenced directly by their git URL:

```sh
smlpkg add github.com/sjqtentacles/sml-lisp
smlpkg sync
```

This downloads the library into `lib/github.com/sjqtentacles/sml-lisp/`.
Reference it from your own `.mlb` with a relative path to `lisp.mlb`:

```
lib/github.com/sjqtentacles/sml-lisp/lisp.mlb
```

For Poly/ML, `use` the sources in order:

```sml
use "lib/github.com/sjqtentacles/sml-lisp/lisp.sig";
use "lib/github.com/sjqtentacles/sml-lisp/lisp.sml";
```

## Usage

`run` reads a whole program and returns the printed value of the last form:

```sml
Lisp.run "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 20)"
(* "2432902008176640000" *)

Lisp.run
  "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))         \
  \         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))        \
  \  (even? 1000000))"
(* "#t"  -- runs in constant stack thanks to proper tail calls *)
```

Lower-level use of the reader and evaluator:

```sml
val v   = Lisp.read "(+ 1 2)"          (* a parsed datum *)
val env = Lisp.baseEnv ()              (* primitives bound *)
val r   = Lisp.eval (v, env)           (* Lisp.Int 3 *)
val s   = Lisp.print r                 (* "3" *)
```

## API

- `read : string -> value`     parse exactly one datum
- `readAll : string -> value list`  parse a whole program
- `print : value -> string`    render a value as s-expression text
- `baseEnv : unit -> env`      fresh global env with primitives
- `eval : value * env -> value`  evaluate (proper tail calls)
- `run : string -> string`     read + eval a program, return last result
- `exception LispError of string`

## Project layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      build + test
lib/github.com/sjqtentacles/sml-lisp/
  lisp.sig                                    the LISP signature
  lisp.sml                                    reader + evaluator
  lisp.mlb                                    MLB for consumers
test/
  test.mlb                                    test basis (MLton)
  test.sml                                    assertion suite
.github/workflows/ci.yml                      CI (MLton + Poly/ML)
```

## License

MIT. See [LICENSE](LICENSE).
