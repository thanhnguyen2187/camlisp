open Camlisp.Tokenizer;;
open Camlisp.Parser;;

while true do
    let line = read_line() in
    (* TODO: see why does blank input raise error *)
    let line = Tokenizer.strip_input line in
    if line != "" then
        let tokens = Tokenizer.tokenize line in
        Parser.parse tokens
        |> Queue.to_seq
        |> (fun nodes ->
                Seq.iter (
                    fun node ->
                        Parser.to_string node
                        |> print_string
                        |> print_newline
                ) nodes)
        |> print_newline
        ;
done

