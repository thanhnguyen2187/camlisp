open Tokenizer

module Parser =
    struct
        type node =
            | None_
            | NumberInt of int
            | NumberFloat of float
            | String_ of string
            | Symbol of string
            | Sequence of node Queue.t
            | Application of node * node Queue.t
            | Define of string * node
            | Func of node Queue.t * node Queue.t
        let rec to_string n =
            match n with
            | None_ -> "null"
            | NumberInt (value) -> string_of_int value
            | NumberFloat (value) -> string_of_float value
            | String_ (value) -> "\"" ^ value ^ "\""
            | Symbol (value) -> value
            | Application (proc, params)
                -> "(" ^ (to_string proc) ^ " "
                ^ (to_string_nodes params true) ^ ")"
            | Define (symbol, value)
                -> "(define " ^ symbol ^ " "
                ^ (to_string value) ^ ">"
            | Sequence (params) -> to_string_nodes params true
            | Func (params, body) ->
                "(lambda " ^ to_string_nodes params true ^ " "
                ^ to_string_nodes body false ^ ")"
            (* | _ -> "<Node undefined>" *)
        and to_string_nodes nodes wrapped =
            Queue.to_seq nodes
            |> (fun iters -> Seq.map to_string iters)
            |> (fun strings -> String.concat ", " (List.of_seq strings))
            |> (fun result ->
                    if wrapped
                    then "(" ^ result ^ ")"
                    else result)

        let number_int_regex = Str.regexp {|^[-+]?[0-9]+$|}
        let number_float_regex = Str.regexp {|^[-+]?[0-9]+\.[0-9]+$|}
        let string_regex = Str.regexp {|^".*"$|}
        let symbol_regex = Str.regexp {|^[a-zA-Z+-/*][a-zA-Z0-9\.]*$|}
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
                NumberInt(int_of_string expr)
            | expr when Str.string_match number_float_regex expr 0 ->
                NumberFloat(float_of_string expr)
            | expr when Str.string_match string_regex expr 0 ->
                (* skipping the first and last character of expr, for
                   example, `"something"` gets turned into `something` *)
                String_(String.sub expr 1 ((String.length expr) - 2))
            | expr when Str.string_match symbol_regex expr 0 ->
                Symbol(expr)
            | _ -> failwith ("parse_one was unable to match expr with a defined pattern: " ^ expr)
        let parse_define nodes =
            let param_1 = Queue.take_opt nodes in
            let param_2 = Queue.take_opt nodes in
            begin
                match param_1, param_2 with
                | Some (Symbol name), Some node -> Define (name, node)
                | _, _ -> failwith ("parse_define received invalid node " ^ to_string (Sequence(nodes)))
                (* | _ -> None_ *)
            end
        let all_symbols nodes =
            Queue.fold
                (fun curr node ->
                    match curr, node with
                    | false, _ -> false
                    | true, Symbol _ -> true
                    | _, _ -> failwith "all_symbols unreachable code")
                true
                nodes
        let parse_lambda nodes =
            let params = Queue.take_opt nodes in
            let body = nodes in
            begin
                match params, body with
                | Some(Sequence(nodes)), _
                when (Queue.length body) > 0 && all_symbols nodes ->
                    Func(nodes, body)
                | _, _ -> failwith ("parse_lambda received an invalid node " ^ to_string (Sequence(nodes)))
            end
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
                let first_node = Queue.take nodes in
                begin
                    match first_node with
                    (* TODO: using Queue means there is implicit mutation;
                             find a way to do it efficiently without mutation *)
                    | Symbol("define") -> parse_define nodes
                    | Symbol("lambda") -> parse_lambda nodes
                    | _ -> Application(first_node, nodes)
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
