open Camlisp.Tokenizer;;

while true do
    let line = read_line()
    in Stack.iter Tokenizer.print_token (Tokenizer.tokenize line);
    ()
done

