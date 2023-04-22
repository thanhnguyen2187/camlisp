open Parser

module Evaluator =
    struct
        let add_int_int param_1 param_2 = param_1 + param_2
        let add_int_float param_1 param_2 = float_of_int param_1 +. param_2
        let add_float_int param_1 param_2 = add_int_float param_2 param_1
        let add_float_float param_1 param_2 = param_1 +. param_2

        let rec add_ints params =
            Seq.fold_left
                (fun curr node ->
                    match node with
                    | Parser.NumberInt(value) -> curr + value
                    | Parser.Application(proc, params) ->
                        begin
                            let result = eval (Parser.Application (proc, params)) in
                            match result with
                            | Parser.NumberInt(value) -> curr + value
                            | _ -> failwith ("invalid node for proc +: " ^ Parser.to_string result)
                        end
                    | _ -> failwith ("invalid node for proc +: " ^ Parser.to_string node))
                0
                (Queue.to_seq params)
        and add_floats params =
            Seq.fold_left
                (fun curr node ->
                    match node with
                    | Parser.NumberFloat(value) -> curr +. value
                    | Parser.Application(proc, params) ->
                        begin
                            let result = eval (Parser.Application (proc, params)) in
                            match result with
                            | Parser.NumberFloat(value) -> curr +. value
                            | _ -> failwith ("invalid node for proc +.: " ^ Parser.to_string result)
                        end
                    | _ -> failwith ("invalid node for proc +.: " ^ Parser.to_string node))
            0.
            (Queue.to_seq params)
        and apply proc params =
            match proc with
            | Parser.Symbol(expr) ->
                begin
                    match expr with
                    | "+" -> Parser.NumberInt(add_ints params) 
                    | "+." -> Parser.NumberFloat(add_floats params)
                    | _ -> failwith "unimplemented yet"
                end
            | _ -> failwith "unimplemented yet"
        and eval node =
            match node with
            | Parser.NumberInt(value) -> Parser.NumberInt(value)
            | Parser.NumberFloat(value) -> Parser.NumberFloat(value)
            | Parser.String_(value) -> Parser.String_(value)
            (* | Parser.Symbol(expr) -> Hashtbl.find env expr *)
            | Parser.Application(proc, params) -> apply proc params
            | _ -> failwith "eval node not implemented"
    end

