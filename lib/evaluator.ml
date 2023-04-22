open Parser

module Evaluator =
    struct
        let default_env : (string, Parser.node) Hashtbl.t = Hashtbl.create 10
        (* let add_binding env expr node = *)
        (*     Hashtbl.add env expr node *)
        (* let remove_binding env expr = *)
        (*     Hashtbl.remove env expr *)
        let rec try_eval_int node =
            match node with
            | Parser.NumberInt(value) -> value
            | Parser.Application(proc, params) ->
                begin
                    let result = apply proc params in
                    match result with
                    | Parser.NumberInt(value) -> value
                    | _ -> failwith ("unable to make an int out of node: " ^ Parser.to_string node)
                end
            | _ -> failwith ("try_eval_int could not eval node to int " ^ Parser.to_string node)
        and make_operator_handler op params =
            Seq.fold_left
                (fun curr node ->
                    let value = try_eval_int node in
                    (op curr value))
                (try_eval_int (Queue.peek params))
                (Queue.to_seq params |> Seq.drop 1)
        and apply proc params =
            match proc with
            | Parser.Symbol(expr) ->
                begin
                    match expr with
                    | "+" -> Parser.NumberInt(make_operator_handler (+) params) 
                    (* | "+." -> Parser.NumberFloat(add_floats params) *)
                    | "-" -> Parser.NumberInt(make_operator_handler (-) params) 
                    | "*" -> Parser.NumberInt(make_operator_handler ( * ) params) 
                    | "/" -> Parser.NumberInt(make_operator_handler (/) params)
                    | _ -> failwith "unimplemented yet"
                end
            | _ -> failwith "unimplemented yet"
        and eval env node =
            match node with
            | Parser.NumberInt(value) -> Parser.NumberInt(value)
            | Parser.NumberFloat(value) -> Parser.NumberFloat(value)
            | Parser.String_(value) -> Parser.String_(value)
            | Parser.Symbol(expr) -> Hashtbl.find env expr
            | Parser.Application(proc, params) -> apply proc params
            | Parser.Define(name, node) ->
                Hashtbl.add env name node;
                node
            | _ -> failwith ("eval node not implemented " ^ Parser.to_string node)
    end

