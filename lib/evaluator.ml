open Parser

module Evaluator =
    struct
        let default_env : (string, Parser.node) Hashtbl.t = Hashtbl.create 10
        let rec try_eval_int env node =
            let result = eval env node in
            match result with
            | Parser.NumberInt(value) -> value
            | _ -> failwith ("try_eval_int cannot make an int out of " ^ Parser.to_string result)
        and make_operator_handler env op params =
            Seq.fold_left
                (fun curr node ->
                    let value = try_eval_int env node in
                    (op curr value))
                (try_eval_int env (Queue.peek params))
                (Queue.to_seq params |> Seq.drop 1)
        and apply env proc params =
            (* let () = print_string ( *)
            (*     "apply proc " ^ Parser.to_string proc ^ *)
            (*     " to params " ^ Parser.to_string_nodes params true *)
            (* ); print_newline () in *)
            match proc with
            | Parser.Symbol(expr) ->
                begin
                    match expr with
                    | "+" -> Parser.NumberInt(make_operator_handler env (+) params) 
                    (* | "+." -> Parser.NumberFloat(add_floats params) *)
                    | "-" -> Parser.NumberInt(make_operator_handler env (-) params) 
                    | "*" -> Parser.NumberInt(make_operator_handler env ( * ) params) 
                    | "/" -> Parser.NumberInt(make_operator_handler env (/) params)
                    | _ -> apply
                            env
                            (eval env proc)
                            (* (Seq.map *)
                            (*     (fun node -> (eval env node)) *)
                            (*     (Queue.to_seq params) *)
                            (*  |> Queue.of_seq) *)
                            params
                end
            | Parser.Func(fn_params, body) ->
                (* TODO: optimize by looking at the way `Hashtbl.add` works *)
                let new_env = Hashtbl.copy env in
                Seq.iter2
                    (fun fn_param param ->
                        match fn_param with
                        | Parser.Symbol(fn_param_sym) ->
                            Hashtbl.add new_env fn_param_sym (eval env param)
                        | _ -> failwith "apply error")
                    (Queue.to_seq fn_params)
                    (Queue.to_seq params);
                let rec f body =
                    let node = Queue.take body in
                    let result = eval new_env node in
                    if Queue.length body = 0
                    then result
                    else f body
                in f (Queue.copy body)
            | _ -> failwith (
                "apply is not implemented for proc " ^ Parser.to_string proc ^
                ", params " ^ Parser.to_string_nodes params true
            )
        and eval env node =
            match node with
            | Parser.NumberInt(_) -> node
            | Parser.NumberFloat(_) -> node
            | Parser.String_(_) -> node
            | Parser.Symbol(name) -> Hashtbl.find env name
            | Parser.Sequence(nodes) ->
                (* A hairy bug was found here: `Queue.take` mutates nodes and
                   make the first node/the operator "disappear". As a result,
                   later application is not applicable anymore. `Queue.copy` is
                   used as a counter-measure.

                   TODO: change every underlying types to using `List`, as it is
                   immutable and easier for pattern-maching comparing to `Queue`
                   *)
                let nodes = Queue.copy nodes in
                let proc = Queue.take nodes in
                let params = nodes in
                apply env proc params
            | Parser.Define(name, node) ->
                let node_result = eval env node in
                Hashtbl.add env name node_result;
                node_result
            | Parser.Func(_, _) -> node
            | _ -> failwith ("eval node not implemented " ^ Parser.to_string node)
    end

