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
        |> List.iter
            (fun node_string ->
                print_string node_string;
                print_newline ())

type specialist_t = (string * Arg.spec * string) list

let repeat_str s n =
    let rec iterate s result n =
        if n = 0
        then result
        else iterate s (result ^ s) (n - 1)
    in
    iterate s "" n

(** [reformat_arguments] pad spaces to specialist's arguments. It is needed for
    pretty printing purpose. For example, without the padding:

    [{
    Arguments:

      --arg1 Argument 1
      --long_arg_2 Longer text for argument 2
    }]

    With the padding:

    [{
    Arguments:

      --arg1        Argument 1
      --long_arg_2  Longer text for argument 2
    }]

    Instead of modifying the arguments (--arg1 and --arg2), this way is chosen
    since adding spaces to the arguments render them not working. ([arg1] does
    work, while [arg1   ] does not). *)
let reformat_arguments (specialist : specialist_t) : specialist_t =
    let arg_name_max_width =
        List.fold_left
            (fun curr_max width ->
                if width > curr_max
                then width
                else curr_max)
            0
            (List.map
                (fun (arg, _, _) -> String.length arg)
                specialist)
    in
    List.map
        (fun (arg, spec, desc) ->
            let padding_length = arg_name_max_width - (String.length arg) in
            let padded_desc = repeat_str " " padding_length in
            (arg, spec, padded_desc ^ " " ^ desc))
        specialist

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
let specialist : specialist_t = [
    ("--files", Arg.Set_string files_str, "Files to be evaluated");
    ("--interactive", Arg.Set interactive, "Start the REPL");
    ("--help", Arg.Set_string dummy, "");
    ("-help", Arg.Set_string dummy, "");
]

let () =
    Arg.parse (reformat_arguments specialist) (function _ -> ()) usage_msg;
    if !interactive
    then
        let env = Evaluator.default_env in
        while true do
            read_eval_print env (Prompting "=>") ""
        done
