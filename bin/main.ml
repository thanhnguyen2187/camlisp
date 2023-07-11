let () =
    Arg.parse (Cli.reformat_arguments Cli.speclist) Cli.handle_path Cli.usage_msg;
    if !Cli.interactive
    then Repl.start ()
    else if !Cli.compile
    then print_string "Not implemented yet!"
    
