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
        let is_balanced text =
            let stack = Stack.create () in
            let () = String.iter
                (fun char ->
                    match char with
                    | '(' -> Stack.push char stack
                    | ')' -> let _ = Stack.pop stack in ()
                    | _ -> ())
                text
            in (Stack.length stack) = 0
        let strip_input text =
            Str.replace_first (Str.regexp "[ \t\r\n]*$") "" text
        let tokenize text =
            let rec f tokens expr text =
            let n = String.length text in
            if n = 0 then
                begin
                    if expr != ""
                    then (Word expr) :: tokens
                    else tokens
                end
            else
                let ch = String.sub text 0 1 in
                let rest_text = String.sub text 1 (if n = 1 then 0 else n - 1) in
                match ch with
                | "(" -> f (OpeningBracket :: tokens) expr rest_text
                | ")" ->
                    if expr != ""
                    then f (ClosingBracket :: (Word expr) :: tokens) "" rest_text
                    else f (ClosingBracket :: tokens) "" rest_text
                | " " | "\n" | "\t" ->
                    if expr != ""
                    then f ((Word expr) :: tokens) "" rest_text
                    else f tokens "" rest_text
                | c ->
                    f tokens (expr ^ c) rest_text
            in f [] "" text |> List.rev
    end

