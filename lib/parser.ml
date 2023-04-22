open Tokenizer

module Parser =
    struct
        type node =
            | None_
            | NumberInt of int
            | NumberFloat of float
            | String_ of string
            | Symbol of string
            | Application of node * node Queue.t
        let rec to_string n =
            match n with
            | None_ -> "<Node None>"
            | NumberInt (value) -> "<Node Int: " ^ string_of_int value ^ ">"
            | NumberFloat (value) -> "<Node Float: " ^ string_of_float value ^ ">"
            | String_ (value) -> "<Node String: \"" ^ value ^ "\">"
            | Symbol (value) -> "<Node Symbol: " ^ value ^ ">"
            | Application (proc, params)
                -> "<Node Application: " ^ (to_string proc) ^ "; "
                ^ (to_string_nodes params) ^ ">"
        and to_string_nodes nodes =
            Queue.to_seq nodes
            |> (fun iters -> Seq.map to_string iters)
            |> (fun strings -> String.concat ", " (List.of_seq strings))

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
                   example `"something"` gets turned into `something` *)
                String_(String.sub expr 1 ((String.length expr) - 2))
            | expr when Str.string_match symbol_regex expr 0 ->
                Symbol(expr)
            | _ -> failwith ("parse_one was unable to match expr with a defined pattern: " ^ expr)
        let rec parse_one curr tokens =
            let token = Queue.take tokens in
            match curr, token with
            | None_, Tokenizer.Word(expr) -> parse_expr expr
            | None_, Tokenizer.OpeningBracket ->
                let params = Queue.create () in
                parse_one (Application (None_, params)) tokens
            | Application(None_, params), Tokenizer.Word(expr) ->
                let proc = parse_expr expr in
                parse_one (Application (proc, params)) tokens
            | Application(proc, params), Tokenizer.Word(expr) ->
                let param = parse_expr expr in
                Queue.add param params;
                parse_one (Application (proc, params)) tokens
            | Application(proc, params), Tokenizer.OpeningBracket ->
                let appl = parse_one (Application (None_, Queue.create ())) tokens in
                Queue.add appl params;
                parse_one (Application (proc, params)) tokens
            | Application(proc, params), Tokenizer.ClosingBracket ->
                Application(proc, params)
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
