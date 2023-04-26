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
            let queue = Queue.create() in
            let rec f expr text =
            let n = String.length text in
            if n = 0 then
                begin
                    if expr != ""
                    then Queue.add (Word expr) queue;
                    queue
                end
            else
                let ch = String.sub text 0 1 in
                let rest_text = String.sub text 1 (if n = 1 then 0 else n - 1) in
                match ch with
                | "(" ->
                    Queue.push OpeningBracket queue;
                    f expr rest_text
                | ")" ->
                    if expr != ""
                    then Queue.push (Word expr) queue;
                    Queue.push ClosingBracket queue;
                    let new_expr = (if expr != "" then "" else expr) in
                    f new_expr rest_text
                | " " | "\n" | "\t" ->
                    if expr != ""
                    then Queue.push (Word expr) queue;
                    f "" rest_text
                | c ->
                    f (expr ^ c) rest_text
            in f "" text
    end

