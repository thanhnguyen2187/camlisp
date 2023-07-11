type speclist_t = (string * Arg.spec * string) list

let repeat_str s n =
    let rec iterate s result n =
        if n = 0
        then result
        else iterate s (result ^ s) (n - 1)
    in
    iterate s "" n

(** [reformat_arguments] pad spaces to specialist's arguments. It is needed for
    pretty printing purpose. For example, without the padding:

    {[
    Arguments:

      --arg1 Argument 1
      --long_arg_2 Longer text for argument 2
    ]}

    With the padding:

    {[
    Arguments:

      --arg1        Argument 1
      --long_arg_2  Longer text for argument 2
    ]}

    Instead of modifying the arguments ([--arg1] and [--arg2]), this way is chosen
    since adding spaces to the arguments render them not working ([arg1] does
    work, while [arg1   ] does not). *)
let reformat_arguments (speclist : speclist_t) : speclist_t =
    let arg_name_max_width =
        List.fold_left
            (fun curr_max width ->
                if width > curr_max
                then width
                else curr_max)
            0
            (List.map
                (fun (arg, _, _) -> String.length arg)
                speclist)
    in
    List.map
        (fun (arg, spec, desc) ->
            let padding_length = arg_name_max_width - (String.length arg) in
            let padded_desc = repeat_str " " padding_length in
            (arg, spec, padded_desc ^ " " ^ desc))
        speclist

let usage_msg = {|camlisp

A simple Lisp/Scheme interpreter in OCaml.

Sample usages:
  
  # start an interactive REPL
  camlisp
  # or specify `--interactive`
  camlisp --interactive
  # with some files preloaded
  camlisp --interactive /tmp/a.scm /tmp/b.scm

  # compile some files to `output.ml`
  camlisp --compile /tmp/a.scm /tmp/b.scm
  # specify the target file name
  camlisp \
      --compile /tmp/a.scm /tmp/b.scm \
      --output other_output.ml

Arguments:
|}
let file_paths : string list ref = ref []
let compile = ref false
let output = ref "output.ml"
let interactive = ref false
(* this is needed since `Arg` does not expose a way to print `usage_msg` along
   with `speclist` *)
let print_all usage_msg speclist =
    print_endline usage_msg;
    List.iter
        (fun (arg, _, desc) ->
            print_string "  ";
            print_string arg;
            print_string " ";
            print_endline desc;
        )
        speclist

let handle_path path =
    file_paths := (List.append !file_paths [path]);
    ()

(* TODO: replace `Arg` with something else, as trying to pretty print the
         arguments makes the code needlessly complicated. *)
let rec speclist : speclist_t = [
    ("--interactive", Arg.Set interactive, "Start the REPL");
    ("--compile", Arg.Set compile, "Compile the input files");
    ("--output", Arg.Set_string output , "Compilation output file path; defaults to output.ml");
    ("--help", Arg.Unit (fun () ->
        print_all usage_msg (reformat_arguments speclist);
        raise (Arg.Help "");
    ), "");
    ("-help", Arg.Unit (fun () ->
        print_all usage_msg (reformat_arguments speclist);
        raise (Arg.Help "");
    ), "");
]
