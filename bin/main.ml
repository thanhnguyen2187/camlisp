open Camlisp.Tokenizer
open Camlisp.Parser
open Camlisp.Evaluator

let () =
let env = Evaluator.default_env in
while true do
    let line = read_line() in
    let line = Tokenizer.strip_input line in
    if line != "" then
        let tokens = Tokenizer.tokenize line in
        Parser.parse tokens
        |> Queue.to_seq
        |> (fun nodes ->
                Seq.iter
                (fun node ->
                    (Evaluator.eval env node)
                    |> Parser.to_string
                    |> print_string
                    |> print_newline)
                nodes)
        ;
done

