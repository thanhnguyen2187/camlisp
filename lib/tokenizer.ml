exception InvalidCharacter of string

module Tokenizer =
    struct
        type token =
            | OpeningBracket
            | ClosingBracket
            | Word of string
        let print_token token =
            match token with
            | OpeningBracket ->
                print_string "<Opening Bracket: (>"
            | ClosingBracket ->
                print_string "<Closing Bracket: )>"
            | Word(expr) ->
                print_string ("<Word: " ^ expr ^ ">")
        let tokenize text =
            let stack = Stack.create() in
            let rec f expr text =
            let n = String.length text in
            if n = 0 then
                begin
                    if expr != ""
                    then Stack.push (Word expr) stack;
                    stack
                end
            else
                let ch = String.sub text 0 1 in
                let rest_text = String.sub text 1 (if n = 1 then 0 else n - 1) in
                match ch with
                | "(" ->
                    Stack.push OpeningBracket stack;
                    f expr rest_text
                | ")" ->
                    if expr != ""
                    then Stack.push (Word expr) stack;

                    Stack.push ClosingBracket stack;
                    let new_expr = (if expr != "" then "" else expr) in
                    f new_expr rest_text
                | " " | "\n" | "\t" ->
                    if expr != ""
                    then Stack.push (Word expr) stack;
                    f "" rest_text
                | c ->
                    f (expr ^ c) rest_text
            in f "" text
    end

