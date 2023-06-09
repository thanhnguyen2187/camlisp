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

Run unit tests:

```shell
dune test
```

## TODO

- [x] Implement `+`, `-`, `*`, and `/`
  - [ ] Redefine the underlying type to make the operators work for both `int`
    and `float`
  - [ ] Find a strategy to turn the operators into a "normal" `Func`?
- [x] Implement `lambda`
  - [x] Add dot notation
- [x] Implement `define`
  - [x] Add dot notation
- [x] Implement `cons`, `car` and `cdr`
- [x] Implement `quote`
- [x] Implement `let` and `let*` (done since `let` acts like `let*` in this case
  for the way `env` works)
- [x] Implement `set!`
- [ ] Implement `eval` and `apply`
- [ ] Research how to release/distribute the built binary
- [ ] Improve error handling by separating user's error to internal evaluator
  error
- [ ] Implement a "proper" CLI experience:
  - [x] Interactive mode `--interactive`
  - [ ] Interactive mode with files preloading
  - [ ] Compiling
- [x] Add tests
  - [x] `Tokenizer.is_balance`
  - [x] `Tokenizer.tokenize`
  - [x] `Parser.parse_expr`
  - [x] `Parser.parse_lambda`
  - [x] `Parser.parse_define`
  - [x] `Parser.parse_if`
  - [x] `Parser.parse_let`
  - [x] `Parser.parse_one`
  - [x] `Parser.parse`
  - [x] `Evaluator.compare_nodes`
  - [x] `Evaluator.is_self_eval`
  - [x] `Evaluator.try_eval_int`
  - [x] `Evaluator.make_operator_handler`
  - [x] `Evaluator.apply`
  - [x] `Evaluator.eval`

