open Camlisp.Tokenizer;;
open Camlisp.Parser;;
open Camlisp.Evaluator;;

while true do
    let line = read_line() in
    (* TODO: see why does blank input raise error *)
    let line = Tokenizer.strip_input line in
    if line != "" then
        let tokens = Tokenizer.tokenize line in
        Parser.parse tokens
        |> Queue.to_seq
        |> (fun nodes -> Seq.iter (
                fun node ->
                    (Evaluator.eval node)
                    |> Parser.to_string
                    |> print_string
                    |> print_newline
            ) nodes)
        ;
done

