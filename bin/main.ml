open Camlisp.Tokenizer;;
open Camlisp.Parser;;

while true do
    let line = read_line() in
    (* TODO: see why does blank input raise error *)
    let line = Tokenizer.strip_input line in
    if line != "" then
        let tokens = Tokenizer.tokenize line in
        tokens
        |> (fun tokens -> Parser.parse_one Parser.None_ tokens) 
        |> Parser.to_string
        |> print_string;

    print_newline ();
done

