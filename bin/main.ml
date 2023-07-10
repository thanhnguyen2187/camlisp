open Camlisp

type input_state =
    | Prompting of string
    | Done

let rec read_eval_print env state curr =
    match state with
    | Prompting (characters) ->
        print_string (characters ^ " ");
        let line = read_line () in
        let expr = curr ^ " " ^ line in
        (* TODO: handle the case where prompting gets into an infinite loop when
                 there are redundant closing brackets *)
        if Tokenizer.is_balanced expr
        then read_eval_print env Done expr
        else read_eval_print env (Prompting "..") expr
    | Done ->
        Evaluator.eval_str env curr
        |> List.map Parser.to_string
        |> List.iter
            (fun node_string ->
                print_string node_string;
                print_newline ())

let usage_msg = {|camlisp

A simple Lisp/Scheme interpreter in OCaml.

Sample usages:
  
  # start an interactive REPL
  camlisp
  # or specify `--interactive`
  camlisp --interactive

  # evaluate each files
  camlisp --files /tmp/a.scm /tmp/b.scm

Arguments:
|}
let files_str = ref ""
let interactive = ref true
let dummy = ref ""
let specialist = [
    ("--files       ", Arg.Set_string files_str, "Files to be evaluated");
    ("--interactive ", Arg.Set interactive, "Start the REPL");
    ("--help", Arg.Set_string dummy, "");
    ("-help", Arg.Set_string dummy, "");
]

let () =
    Arg.parse specialist (function _ -> ()) usage_msg;
    if !interactive
    then
        let env = Evaluator.default_env in
        while true do
            read_eval_print env (Prompting "=>") ""
        done
