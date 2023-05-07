open Ppx_compare_lib.Builtin
open Sexplib.Std


module Tokenizer =
    struct
        type token =
            | OpeningBracket
            | QuotedOpeningBracket
            | ClosingBracket
            | Word of string
            [@@deriving compare, sexp]
        let to_string token =
            match token with
            | OpeningBracket -> "(token opening-bracket)"
            | QuotedOpeningBracket -> "(token quoted-opening-bracket)"
            | ClosingBracket -> "(token closing-bracket)"
            | Word(expr) -> Format.sprintf "(token \"%s\")" expr

        (* TODO: improve the function by letting it receive `token`s instead of
           raw text *)
        let is_balanced text =
            let stack = Stack.create () in
            let rec f text =
                let n = String.length text in
                if n = 0
                then Stack.length stack = 0
                else
                    let char = text.[0] in
                    let rest_text = (String.sub text 1 (n - 1)) in
                    match char with
                    | '(' ->
                        Stack.push char stack;
                        f rest_text
                    | ')' ->
                        begin
                            match Stack.pop_opt stack with
                            | Some('(')-> f rest_text
                            | _ -> false
                        end
                    | _ -> f rest_text
            in f text
        let%test "is_balanced__blank_input" =
            is_balanced "" = true
        let%test "is_balanced__good_input" =
            is_balanced "()" = true
        let%test "is_balanced__missing_)" =
            (is_balanced "((") = false
        let%test "is_balanced__missing_(" =
            (is_balanced "))") = false

        let tokenize text =
            let rec f tokens expr text =
            let n = String.length text in
            if n = 0 then
                if expr != ""
                then (Word expr) :: tokens
                else tokens
            else
                let ch = String.sub text 0 1 in
                let rest_text = String.sub text 1 (if n = 1 then 0 else n - 1) in
                match ch with
                | "(" ->
                    if expr = "'"
                    then f (QuotedOpeningBracket :: tokens) "" rest_text
                    else f (OpeningBracket :: tokens) expr rest_text
                | ")" ->
                    if expr != ""
                    then f (ClosingBracket :: Word expr :: tokens) "" rest_text
                    else f (ClosingBracket :: tokens) "" rest_text
                | " " | "\n" | "\t" ->
                    if expr != ""
                    then f (Word expr :: tokens) "" rest_text
                    else f tokens "" rest_text
                | c ->
                    f tokens (expr ^ c) rest_text
            in f [] "" text |> List.rev
        let%test "tokenize__blank_input" =
            tokenize "" = []
        let%test "tokenize__brackets" =
            tokenize "()" = [OpeningBracket; ClosingBracket]
        let%test "tokenize__wrapped_word" =
            tokenize "(word)" = [OpeningBracket; Word "word"; ClosingBracket]
        let%test "tokenize__quoted_bracket" =
            tokenize "'(word)" = [QuotedOpeningBracket; Word "word"; ClosingBracket]
        let%test_unit "tokenize__complex_case" =
            [%test_eq: token list]
            (tokenize "'(f (+ x 1) y '())")
            [
                QuotedOpeningBracket;
                Word "f";
                OpeningBracket;
                Word "+";
                Word "x";
                Word "1";
                ClosingBracket;
                Word "y";
                QuotedOpeningBracket;
                ClosingBracket;
                ClosingBracket;
            ]
    end

