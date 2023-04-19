open Camlisp.Tokenizer;;

while true do
    let line = read_line()
    in Queue.iter Tokenizer.print_token (Tokenizer.tokenize line);
    ()
done

