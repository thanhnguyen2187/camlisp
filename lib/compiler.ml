open Ppx_compare_lib.Builtin
open Sexplib.Std

type ocaml_expr =
    | Int of int
    | Let of {name: string}
    [@@deriving compare, sexp]

let compile node =
    if Evaluator.is_self_eval node
    then (
        match node with
        (* | Parser.Bool (_) *)
        (* | Parser.Func (_, _) *)
        | Parser.NumberInt (value) -> Int value
        (* | Parser.NumberFloat (_) *)
        (* | Parser.String_ (_) -> true *)
        | _ -> failwith "compile unreachable code"
    )
    else failwith "compile unreachable code"
let%test_unit "compile" =
    ([%test_eq: ocaml_expr]
        (compile (Parser.NumberInt 2))
        (Int 2))
