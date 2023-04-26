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
            | Sequence of node list
            | Define of string * node
            | Func of node list * node list
            | If of node * node * node
        let rec to_string n =
            match n with
            | None_ -> "null"
            | NumberInt (value) -> string_of_int value
            | NumberFloat (value) -> string_of_float value
            | String_ (value) -> "\"" ^ value ^ "\""
            | Symbol (value) -> value
            | Define (symbol, value)
                -> "(" ^ symbol ^ " "
                ^ (to_string value) ^ ">"
            | Sequence (params) -> to_string_nodes params true
            | Func (params, body) ->
                "(lambda " ^ to_string_nodes params true ^ " "
                ^ to_string_nodes body false ^ ")"
            | If (pred, conseq, alt) ->
                    "(if " ^ to_string pred ^ " "
                    ^ to_string conseq ^" "
                    ^ to_string alt ^ ")"
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
        let symbol_regex = Str.regexp {|^[a-zA-Z+-/*=][a-zA-Z0-9\.]*$|}
        (* bool_regex needs extra slash (\), since OCaml syntax for raw string
           does not get the pipe (|) without the slash *)
        let bool_regex = Str.regexp {|^true\|false\|#t\|#f$|}
        let pop_until_match stack =
            (* let stack = Stack.copy stack in *)
            let queue = Queue.create () in
            let rec f () =
                let top = Stack.pop stack in
                match top with
                | Tokenizer.ClosingBracket -> queue
                | _ ->
                    Queue.add top queue;
                    f ()
            in f ()
        let parse_expr expr =
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
                Symbol (expr)
            | _ -> failwith ("parse_one was unable to match expr with a defined pattern: " ^ expr)
        let all_symbols nodes =
            Queue.fold
                (fun curr node ->
                    match curr, node with
                    | false, _ -> false
                    | true, Symbol _ -> true
                    | _ -> failwith "all_symbols unreachable code")
                true
                nodes
        let parse_lambda nodes =
            match nodes with
            | Symbol "lambda" :: Sequence params :: body -> Func (params, body)
            | _ -> failwith ("parse_lambda received an invalid node " ^ to_string_nodes nodes true)
        let parse_define nodes =
            match nodes with
            | Symbol "define" :: Symbol name :: node :: [] -> Define (name, node)
            | _ -> failwith ("parse_define received an invalid node " ^ to_string_nodes nodes true)
        let parse_if nodes =
            match nodes with
            | Symbol "if" :: pred :: conseq :: alt :: [] -> If(pred, conseq, alt)
            | _ -> failwith ("parse_if received an invalid node " ^ to_string (Sequence nodes))
        let rec parse_one curr tokens =
            match curr, tokens with
            | None_, Tokenizer.Word(expr) :: rest_tokens ->
                parse_expr expr, rest_tokens
            | None_, Tokenizer.OpeningBracket :: rest_tokens ->
                parse_one (Sequence []) rest_tokens
            | Sequence (nodes), Tokenizer.Word(expr) :: rest_tokens ->
                let node = parse_expr expr in
                parse_one (Sequence (nodes @ [node])) rest_tokens
            | Sequence (nodes), Tokenizer.OpeningBracket :: rest_tokens ->
                let appl, rest_tokens = parse_one (Sequence []) rest_tokens in
                parse_one (Sequence (nodes @ [appl])) rest_tokens
            | Sequence (nodes), Tokenizer.ClosingBracket :: rest_tokens ->
                begin
                    match nodes with
                    | Symbol("define") :: _ -> (parse_define nodes), rest_tokens
                    | Symbol("if") :: _ -> (parse_if nodes), rest_tokens
                    | Symbol("lambda") :: _ -> (parse_lambda nodes), rest_tokens
                    | _ -> curr, rest_tokens
                end
            | _, _ -> failwith "parse_one unreachable code"
        let rec parse tokens =
            match tokens with
            | [] -> []
            | _ ->
                let node, rest_tokens = parse_one None_ tokens
                in node :: parse rest_tokens
    end
;;

