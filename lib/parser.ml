open Tokenizer

module Parser =
    struct
        type node =
            | None_
            | NumberInt of int
            | NumberFloat of float
            | String_ of string
            | Symbol of string
            | Application of node * node list
        let to_string n = match n with
            | None_ -> "<Node None>"
            | NumberInt(value) -> "<Node Int: " ^ string_of_int value ^ ">"
            | NumberFloat(value) -> "<Node Float: " ^ string_of_float value ^ ">"
            | _ -> "<Node unimplemented>"

        let number_int_regex = Str.regexp {|^[-+]?[0-9]+$|}
        let number_float_regex = Str.regexp {|^[-+]?[0-9]+\.[0-9]+$|}
        let string_regex = Str.regexp "^\".*\"$"
        let symbol_regex = Str.regexp "^[a-zA-Z]\\w?$"
        let pop_until_match stack =
            let stack = Stack.copy stack in
            let curr = Queue.create () in
            let rec f () =
                let top = Stack.pop stack in
                match top with
                | Tokenizer.ClosingBracket -> curr
                | _ ->
                    Queue.add top curr;
                    f ()
            in f ()
        let parse_one tokens =
            let curr = None_ in
            let token = Queue.top tokens in
            match curr, token with
            | None_, Tokenizer.Word(expr) ->
                begin
                    match expr with
                    | expr when Str.string_match number_int_regex expr 0 ->
                        NumberInt(int_of_string expr)
                    | expr when Str.string_match number_float_regex expr 0 ->
                        NumberFloat(float_of_string expr)
                    | _ -> None_
                end
            | _, _ -> None_
            (* | Application(None_, _), Tokenizer.Word(expr) -> None_ *)
            (* | Application(Symbol, params), Tokenizer.Word(expr) -> None_ *)
    end
;;


Str.string_match (Str.regexp {|^[-+]?[0-9]+\.[0-9]+$|}) "12.24" 0;;
