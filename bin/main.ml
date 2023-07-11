let () =
    Arg.parse (Cli.reformat_arguments Cli.speclist) (function _ -> ()) Cli.usage_msg;
    if !Cli.interactive
    then Repl.start ()
