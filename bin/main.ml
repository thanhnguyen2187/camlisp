let () =
    Arg.parse (Cli.reformat_arguments Cli.specialist) (function _ -> ()) Cli.usage_msg;
    if !Cli.interactive
    then Repl.start ()
