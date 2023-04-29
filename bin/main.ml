open Camlisp.Tokenizer
open Camlisp.Parser
open Camlisp.Evaluator

type input_state =
    | Prompting of string
    | Done

let rec read_eval_print env state curr =
    match state with
    | Prompting (characters) ->
        print_string (characters ^ " ");
        let line = read_line () in
        let expr = curr ^ " " ^ line in
        if Tokenizer.is_balanced expr
        then read_eval_print env Done expr
        else read_eval_print env (Prompting "..") expr
    | Done ->
        Tokenizer.tokenize curr
        (* |> List.map Tokenizer.to_string *)
        (* |> List.iter print_string *)
        (* |> print_newline *)
        |> Parser.parse
        (* |> List.to_seq *)
        |> Evaluator.eval_nodes env
        |> Seq.map Parser.to_string
        |> Seq.iter
            (fun node_string ->
                print_string node_string;
                print_newline ())

let () =
let env = Evaluator.default_env in
while true do
    read_eval_print env (Prompting "=>") ""
done
