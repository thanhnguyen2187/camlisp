open Parser

module Evaluator =
    struct
        let compare_nodes n1 n2 =
            match n1, n2 with
            | Parser.Bool(v1), Parser.Bool(v2) -> v1 = v2
            | Parser.NumberInt(v1), Parser.NumberInt(v2) -> v1 = v2
            | _ -> false
        let default_env : (string, Parser.node) Hashtbl.t = Hashtbl.create 10
        let is_self_eval = function
            | Parser.Bool (_)
            | Parser.Func (_, _)
            | Parser.NumberInt (_)
            | Parser.NumberFloat (_)
            | Parser.String_ (_) -> true
            | _ -> false
        let rec try_eval_int env node =
            let result = eval env node in
            match result with
            | Parser.NumberInt(value) -> value
            | _ -> failwith ("try_eval_int cannot make an int out of " ^ Parser.to_string node)
        and make_operator_handler env op params =
            Seq.fold_left
                (fun curr node ->
                    let value = try_eval_int env node in
                    (op curr value))
                (try_eval_int env (List.hd params))
                (List.to_seq params |> Seq.drop 1)
        and apply env proc params =
            (* let () = print_string ( *)
            (*     "apply proc " ^ Parser.to_string proc ^ *)
            (*     " to params " ^ Parser.to_string_nodes params true *)
            (* ); print_newline () in *)
            match proc with
            | Parser.Symbol(expr) ->
                begin
                    match expr with
                    | "quote" ->
                        begin
                            match params with
                            | node :: [] -> node
                            | _ -> failwith ("apply receive invalid params for quote " ^ (Parser.to_string_nodes params true))
                        end
                    | "+" -> Parser.NumberInt(make_operator_handler env (+) params) 
                    | "-" -> Parser.NumberInt(make_operator_handler env (-) params) 
                    | "*" -> Parser.NumberInt(make_operator_handler env ( * ) params) 
                    | "/" -> Parser.NumberInt(make_operator_handler env (/) params)
                    | "=" ->
                        let first_param = eval env (List.hd params) in
                        begin match Seq.find
                            (fun param -> not (compare_nodes (eval env param) first_param))
                            (* skip the first parameter to avoid redundant
                               `eval` *)
                            (List.to_seq params |> Seq.drop 1)
                            with
                                | Some(_) -> Parser.Bool(false)
                                | None -> Parser.Bool(true)
                        end
                    | _ -> apply env (eval env proc) params
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
                    (List.to_seq fn_params)
                    (List.to_seq params);
                let rec f body =
                    begin
                        match body with
                        | node :: [] -> eval new_env node
                        | node :: rest_body ->
                            let _ = eval new_env node in
                            f rest_body
                        | _ -> failwith "apply cannot work with empty body"
                    end
                in f body
            | _ -> failwith (
                "apply is not implemented for proc " ^ Parser.to_string proc ^
                ", params " ^ Parser.to_string_nodes params true
            )
        and eval env node =
            match node with
            | _ when is_self_eval node -> node

            (* other node types *)
            | Parser.If (pred, conseq, alt) ->
                begin
                    match (eval env pred) with
                        | Parser.Bool (false) -> eval env alt
                        | _ -> eval env conseq
                end
            | Parser.Symbol name ->
                let node = Hashtbl.find_opt env name in
                begin
                    match node with
                    | Some (node) -> node
                    | None -> failwith ("eval unable to find symbol " ^ name)
                end
            | Parser.Quote node -> node
            | Parser.Sequence nodes ->
                (* A hairy bug was found here: `Queue.take` mutates nodes and
                   make the first node/the operator "disappear". As a result,
                   later application is not applicable anymore. `Queue.copy` is
                   used as a counter-measure.

                   TODO: change every underlying types to using `List`, as it is
                   immutable and easier for pattern-maching comparing to `Queue`
                   *)
                begin
                    match nodes with
                    | proc :: params -> apply env proc params
                    | _ -> failwith ""
                end
                (* let nodes = Queue.copy nodes in *)
                (* let proc = Queue.take nodes in *)
                (* let params = nodes in *)
                (* apply env proc params *)
            | Parser.Define (name, node) ->
                let node_result = eval env node in
                Hashtbl.add env name node_result;
                node_result
            | _ -> failwith ("eval node not implemented " ^ Parser.to_string node)
        and eval_nodes env nodes =
            Seq.map
                (fun node -> eval env node)
                (List.to_seq nodes)
    end

