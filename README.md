# CamLisp (WIP)

A toy Lisp/Scheme interpreter in OCaml.

## Getting Started

Make sure that you have `dune` and `opam` ready.

```shell
dune --version
# 3.7.1
opam --version
# 2.1.3
```

Install dependencies:

```shell
...
```

Run the binary:

```shell
dune exec camlisp
```

Test the REPL:

```lisp
=> 1
1
=> (define x 1)
1
=> (+ x 1)
2
```

```lisp
=> (define (fact n)
..   (if (= n 1)
..     1
..     (* n (fact (- n 1)))))
(lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))
=> (fact 5)
120
```

```lisp
=> (define (fact-iter n)
..   (define (iter result n)
..     (if (= n 1)
..       result
..       (iter (* result n) (- n 1))))
..   (iter 1 n))
(lambda ...)
=> (fact-iter 5)
120
```

## TODO

- [x] Implement `+`, `-`, `*`, and `/`
  - [ ] Redefine the underlying type to make the operators work for both `int`
    and `float`
  - [ ] Find a strategy to turn the operators into a "normal" `Func`?
- [x] Implement `lambda`
- [x] Implement `define`
- [ ] Implement `cons`, `car` and `cdr`
- [ ] Implement `quote`
- [ ] Implement `let` and `let*`
- [ ] Implement `set!`
- [ ] Implement `eval` and `apply`
- [ ] Research how to release/distribute the built binary

