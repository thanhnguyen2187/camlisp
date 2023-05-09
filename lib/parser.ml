open Ppx_compare_lib.Builtin
open Sexplib.Std

open Tokenizer

module Parser =
    struct
        type node =
            | None_
            | Bool of bool
            | NumberInt of int
            | NumberFloat of float
            | String_ of string
            | Symbol of string
            | Quote of node
            | Pair of node * node
            | Sequence of node list
            | Define of string * node
            | Func of node list * node list
            | Let of node list * node list
            | If of node * node * node
            [@@deriving compare, sexp]
        let rec to_string n =
            match n with
            | None_ -> "null"
            | NumberInt (value) -> string_of_int value
            | NumberFloat (value) -> string_of_float value
            | String_ (value) -> Format.sprintf "\"%s\"" value
            | Symbol (value) -> value
            | Quote (node) -> Format.sprintf "(quote %s)" (to_string node)
            | Pair (node_1, node_2) ->
                Format.sprintf "(%s . %s)" (to_string node_1) (to_string node_2)
            | Define (symbol, value) ->
                Format.sprintf
                    "(define %s %s)"
                    symbol
                    (to_string value)
            | Sequence (params) -> to_string_nodes params true
            | Func (params, body) ->
                Format.sprintf
                    "(lambda %s %s)"
                    (to_string_nodes params true)
                    (to_string_nodes body false)
            | Let (bindings, body) ->
                Format.sprintf
                    "(let %s %s)"
                    (to_string_nodes bindings true)
                    (to_string_nodes body false)
            | If (pred, conseq, alt) ->
                Format.sprintf "(if %s %s %s)"
                    (to_string pred)
                    (to_string conseq)
                    (to_string alt)
            | Bool (value) ->
                if value
                then "#t"
                else "#f"
            (* | _ -> "*undefined*" *)
        and to_string_nodes nodes wrapped =
            List.to_seq nodes
            |> (fun iters -> Seq.map to_string iters)
            |> (fun strings -> String.concat " " (List.of_seq strings))
            |> (fun result ->
                    if wrapped
                    then "(" ^ result ^ ")"
                    else result)

        let number_int_regex = Str.regexp {|^[-+]?[0-9]+$|}
        let number_float_regex = Str.regexp {|^[-+]?[0-9]+\.[0-9]+$|}
        let string_regex = Str.regexp {|^".*"$|}
        (* symbol_regex needs a slash (\) before (!) since without the slash,
           it will not work for `set!` or names like that *)
        let symbol_regex = Str.regexp {|^[a-zA-Z+-/*='][a-zA-Z0-9\.-\!]*$|}
        (* bool_regex needs an extra slash (\), since OCaml syntax for raw
           string does not get the pipe (|) without the slash *)
        let bool_regex = Str.regexp {|^true\|false\|#t\|#f$|}
        let rec parse_expr expr =
            match expr with
            | expr when Str.string_match number_int_regex expr 0 ->
                NumberInt (int_of_string expr)
            | expr when Str.string_match number_float_regex expr 0 ->
                NumberFloat (float_of_string expr)
            | expr when Str.string_match string_regex expr 0 ->
                (* skipping the first and last character of expr, for
                   example, `"something"` gets turned into `something`;
                   `((String.length expr) - 2)` is passed since `.sub` expect
                   the cut's length instead of the end index *)
                String_ (String.sub expr 1 ((String.length expr) - 2))
            | expr when Str.string_match bool_regex expr 0 ->
                Bool (String.contains expr 't')
            | expr when Str.string_match symbol_regex expr 0 ->
                if String.starts_with ~prefix:"'" expr
                then
                    let symbol = parse_expr (String.sub expr 1 ((String.length expr) - 1)) in
                    Quote symbol
                else Symbol expr
            | _ -> failwith ("parse_one was unable to match expr with a defined pattern: " ^ expr)
        let%test_unit "parse_expr__number_int" =
            [%test_eq: node] (parse_expr "123") (NumberInt 123);
            [%test_eq: node] (parse_expr "+123") (NumberInt 123);
            [%test_eq: node] (parse_expr "-456") (NumberInt (-456));
            ()
        let%test_unit "parse_expr__number_float" =
            [%test_eq: node] (parse_expr "1.01") (NumberFloat 1.01);
            [%test_eq: node] (parse_expr "-2.0") (NumberFloat (-2.0));
            ()
        let%test_unit "parse_expr__string" =
            [%test_eq: node] (parse_expr "\"a string\"") (String_ "a string");
            ()
        let%test_unit "parse_expr__bool" =
            ([%test_eq: node] (parse_expr "true") (Bool true));
            ([%test_eq: node] (parse_expr "#t") (Bool true));
            ([%test_eq: node] (parse_expr "false") (Bool false));
            ([%test_eq: node] (parse_expr "#f") (Bool false));
            ()
        let%test_unit "parse_symbol" =
            ([%test_eq: node] (parse_expr "a") (Symbol "a"));
            ([%test_eq: node] (parse_expr "+") (Symbol "+"));
            ([%test_eq: node] (parse_expr "set!") (Symbol "set!"));
            ()

        let parse_lambda nodes =
            match nodes with
            | Symbol "lambda" :: Sequence params :: body ->
                Func (params, body)
            | _ -> failwith ("parse_lambda received an invalid node " ^ to_string_nodes nodes true)
        let parse_define nodes =
            match nodes with
            | Symbol "define" :: Symbol name :: node :: [] ->
                Define (name, node)
            | Symbol "define" :: Sequence ((Symbol name) :: params) :: body ->
                Define (name, Func (params, body))
            | _ ->
                failwith ("parse_define received an invalid node " ^ to_string_nodes nodes true)
        let parse_if nodes =
            match nodes with
            | Symbol "if" :: pred :: conseq :: alt :: [] -> If (pred, conseq, alt)
            | _ -> failwith ("parse_if received an invalid node " ^ to_string (Sequence nodes))
        let parse_let nodes =
            match nodes with
            | Symbol "let" :: Sequence bindings :: body -> Let (bindings, body)
            | _ -> failwith ("parse_let received an invalid node " ^ to_string (Sequence nodes))
        let rec parse_one curr tokens =
            match curr, tokens with
            | None_, Tokenizer.Word(expr) :: rest_tokens ->
                parse_expr expr, rest_tokens
            | None_, Tokenizer.OpeningBracket :: rest_tokens ->
                parse_one (Sequence []) rest_tokens
            | None_, Tokenizer.QuotedOpeningBracket :: rest_tokens ->
                let result, rest_tokens = parse_one (Sequence []) rest_tokens in
                Quote result, rest_tokens
            | Sequence (nodes), Tokenizer.Word(expr) :: rest_tokens ->
                let node = parse_expr expr in
                parse_one (Sequence (nodes @ [node])) rest_tokens
            | Sequence (nodes), Tokenizer.OpeningBracket :: rest_tokens ->
                let appl, rest_tokens = parse_one (Sequence []) rest_tokens in
                parse_one (Sequence (nodes @ [appl])) rest_tokens
            | Sequence nodes, Tokenizer.QuotedOpeningBracket :: rest_tokens ->
                let appl, rest_tokens = parse_one (Sequence []) rest_tokens in
                parse_one (Sequence (nodes @ [Quote appl])) rest_tokens
            | Sequence (nodes), Tokenizer.ClosingBracket :: rest_tokens ->
                begin
                    match nodes with
                    | Symbol "define" :: _ -> (parse_define nodes), rest_tokens
                    | Symbol "if" :: _ -> (parse_if nodes), rest_tokens
                    | Symbol "lambda" :: _ -> (parse_lambda nodes), rest_tokens
                    | Symbol "let" :: _ -> (parse_let nodes), rest_tokens
                    | node_1 :: Symbol "." :: node_2 :: [] ->
                        Pair (node_1, node_2), rest_tokens
                    | _ -> curr, rest_tokens
                end
            | _ -> failwith "parse_one unreachable code"
        let rec parse tokens =
            match tokens with
            | [] -> []
            | _ ->
                let node, rest_tokens = parse_one None_ tokens
                in node :: parse rest_tokens
    end
;;

