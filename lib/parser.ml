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
            | Sequence of node Queue.t
            | Define of string * node
            | Func of node Queue.t * node Queue.t
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
            Queue.to_seq nodes
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
        let parse_define nodes =
            let _ = Queue.take_opt nodes in
            let param_1 = Queue.take_opt nodes in
            let param_2 = Queue.take_opt nodes in
            begin
                match param_1, param_2 with
                | Some (Symbol name), Some node -> Define (name, node)
                | _ -> failwith ("parse_define received invalid node " ^ to_string (Sequence(nodes)))
            end
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
            let _ = Queue.take_opt nodes in
            let params = Queue.take_opt nodes in
            let body = nodes in
            begin
                match params, body with
                | Some (Sequence param_nodes), _ ->
                (* when (Queue.length body) > 0 && all_symbols param_nodes -> *)
                    Func (param_nodes, body)
                | _, _ -> failwith ("parse_lambda received an invalid node " ^ to_string (Sequence nodes))
            end
        let parse_if nodes =
            let _ = Queue.take_opt nodes in
            let pred = Queue.take_opt nodes in
            let conseq = Queue.take_opt nodes in
            let alt = Queue.take_opt nodes in
            match pred, conseq, alt with
            | Some(pred), Some(conseq), Some(alt) -> If(pred, conseq, alt)
            | _ -> failwith ("parse_lambda received an invalid node " ^ to_string (Sequence nodes))
        let rec parse_one curr tokens =
            let token = Queue.take tokens in
            match curr, token with
            | None_, Tokenizer.Word(expr) -> parse_expr expr
            | None_, Tokenizer.OpeningBracket ->
                let nodes = Queue.create () in
                parse_one (Sequence nodes) tokens
            | Sequence (nodes), Tokenizer.Word(expr) ->
                let node = parse_expr expr in
                Queue.add node nodes;
                parse_one curr tokens
            | Sequence (nodes), Tokenizer.OpeningBracket ->
                let appl = parse_one (Sequence (Queue.create ())) tokens in
                Queue.add appl nodes;
                parse_one curr tokens
            | Sequence (nodes), Tokenizer.ClosingBracket ->
                let first_node = Queue.peek nodes in
                begin
                    match first_node with
                    (* TODO: using Queue means there is implicit mutation;
                             find a way to do it efficiently without mutation *)
                    | Symbol("define") -> parse_define nodes
                    | Symbol("lambda") -> parse_lambda nodes
                    | Symbol("if") -> parse_if nodes
                    | _ -> curr
                end
            | _, _ -> failwith "parse_one unreachable code"
        let parse tokens =
            let rec f nodes tokens =
                if (Queue.length tokens) = 0
                then nodes
                else
                    let node = (parse_one None_ tokens) in
                    Queue.add node nodes;
                    f nodes tokens
            in f (Queue.create ()) (Queue.copy tokens)
    end
;;

