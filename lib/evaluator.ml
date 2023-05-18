open Ppx_compare_lib.Builtin
open Sexplib.Std

let default_env : (string, Parser.node) Hashtbl.t = Hashtbl.create 10

let compare_nodes n1 n2 =
    match n1, n2 with
    | Parser.Bool(v1), Parser.Bool(v2) -> v1 = v2
    | Parser.NumberInt(v1), Parser.NumberInt(v2) -> v1 = v2
    | _ -> false
let%test_unit "compare_nodes" =
    [%test_eq: bool]
        (compare_nodes (Parser.Bool true) (Parser.Bool true))
        true;
    [%test_eq: bool]
        (compare_nodes (Parser.Bool false) (Parser.Bool true))
        false;
    [%test_eq: bool]
        (compare_nodes (Parser.NumberInt 1) (Parser.NumberInt 1))
        true;
    [%test_eq: bool]
        (compare_nodes (Parser.NumberInt 3) (Parser.NumberInt 4))
        false;
    ()

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
            | "cons" ->
                begin
                    match params with
                    | param_1 :: param_2 :: [] ->
                        let param_1_evaluated = eval env param_1 in
                        let param_2_evaluated = eval env param_2 in
                        begin
                            match param_1_evaluated, param_2_evaluated with
                            | _, Sequence nodes -> Sequence (param_1_evaluated :: nodes)
                            | _ -> Pair (param_1_evaluated, param_2_evaluated)
                        end
                    | _ -> failwith ("apply receive invalid params for cons " ^ (Parser.to_string_nodes params true))
                end
            | "car" ->
                begin
                    match params with
                    | param :: [] ->
                        begin
                            let param_evaluated = eval env param in
                            match param_evaluated with
                            | Parser.Pair (node, _) -> node
                            | Parser.Sequence (node :: _) -> node
                            | _ -> failwith ("apply received invalid params for car " ^ (Parser.to_string_nodes params true))
                        end
                    | _ -> failwith ("apply received invalid params for car " ^ (Parser.to_string_nodes params true))
                end
            | "cdr" ->
                begin
                    match params with
                    | param :: [] ->
                        begin
                            let param_evaluated = eval env param in
                            match param_evaluated with
                            | Parser.Pair (_, node) -> node
                            | Parser.Sequence (_ :: nodes) -> Parser.Sequence nodes
                            | _ -> failwith ("apply received invalid params for cdr " ^ (Parser.to_string_nodes params true))
                        end
                    | _ -> failwith ("apply received invalid params for cdr " ^ (Parser.to_string_nodes params true))
                end
            | "set!" ->
                begin
                    match params with
                    | Parser.Symbol (name) :: node :: [] ->
                        let result = eval env node in
                        Hashtbl.replace env name result;
                        result
                    | _ -> failwith ("apply received invalid params for set!" ^ Parser.to_string_nodes params true)
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
    | Parser.Let (bindings, body) ->
        begin
            let env_copy = Hashtbl.copy env in
            let rec bind_each env = function
                | [] -> ()
                | (Parser.Sequence ((Symbol name) :: node :: [])) :: rest ->
                    Hashtbl.add env name (eval env node);
                    bind_each env rest
                | _ -> failwith "eval bind_each of let received invalid parameter"
            in
            bind_each env_copy bindings;
            let rec eval_each env = function
                | node :: [] -> eval env node
                | node :: rest_nodes ->
                    let _ = eval env node
                    in eval_each env rest_nodes
                | _ -> failwith "eval eval_each of let received invalid parameter"
            in
            eval_each env_copy body
        end
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
        begin
            match nodes with
            | proc :: params -> apply env proc params
            | _ -> failwith ("eval sequence unreachable code with data " ^ Parser.to_string_nodes nodes true)
        end
    | Parser.Define (name, node) ->
        let node_result = eval env node in
        Hashtbl.add env name node_result;
        node_result
    | _ -> failwith ("eval node not implemented " ^ Parser.to_string node)
and eval_nodes env nodes =
    Seq.map
        (fun node -> eval env node)
        (List.to_seq nodes)

