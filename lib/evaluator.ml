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
let%test_unit "is_self_eval" =
    [%test_eq: bool]
        (is_self_eval (Parser.Bool true))
        true;
    [%test_eq: bool]
        (is_self_eval (Parser.Func ([Symbol "x"], [Symbol "x"])))
        true;
    [%test_eq: bool]
        (is_self_eval (Parser.NumberInt 1))
        true;
    [%test_eq: bool]
        (is_self_eval (Parser.NumberFloat 1.0))
        true;
    [%test_eq: bool]
        (is_self_eval (Parser.String_ "abc"))
        true;
    [%test_eq: bool]
        (is_self_eval (Parser.If (Parser.Bool true, Parser.NumberInt 1, Parser.NumberInt 2)))
        false;
    ()

let transform_dot_params
    (params : Parser.node list)
    : (int * Parser.node list) =
    let rec recur index params result =
        match params with
        | Parser.Symbol "." :: final_param :: [] -> index, final_param :: result
        | param :: rest_params -> recur (index + 1) rest_params (param :: result)
        | [] -> -1, result
    in
    let result_index, result_params = recur 0 params [] in
    result_index, List.rev result_params
let%test_unit "transform_dot_params" =
    [%test_eq: int * Parser.node list]
        (transform_dot_params [])
        (-1, []);
    [%test_eq: int * Parser.node list]
        (transform_dot_params [
            Parser.Symbol "x";
            Parser.Symbol "y";
            Parser.Symbol "z";
        ])
        (-1, [
            Parser.Symbol "x";
            Parser.Symbol "y";
            Parser.Symbol "z";
        ]);
    [%test_eq: int * Parser.node list]
        (transform_dot_params [
            Parser.Symbol "x";
            Parser.Symbol ".";
            Parser.Symbol "xs";
        ])
        (1, [
            Parser.Symbol "x";
            Parser.Symbol "xs";
        ]);
    ()

let transform_dot_args
    (dot_param_index : int)
    (args : Parser.node list)
    : Parser.node list =
    if dot_param_index = -1
    then args
    else if dot_param_index = List.length args
    then List.append args [Parser.Sequence []]
    else
        let rec recur index args result =
            match args with
            | [] -> result
            | arg :: rest_args when index < dot_param_index ->
                recur (index + 1) rest_args (arg :: result)
            | arg :: rest_args when index = dot_param_index ->
                recur (index + 1) rest_args ((Parser.Sequence [arg]) :: result)
            | arg :: rest_args when index > dot_param_index ->
                begin
                    match result with
                    | (Parser.Sequence args_) :: rest_result ->
                        recur (index + 1) rest_args ((Parser.Sequence (arg :: args_)) :: rest_result)
                    | _ -> failwith "transform_dot_params unreachable case 1"
                end
            (* | _ -> result *)
            | _ -> failwith "transform_dot_params unreachable case 2"
        in
        let result = recur 0 args [] in
        match result with
        | (Parser.Sequence vargs) :: args ->
            vargs
            |> List.rev
            |> (fun nodes -> Parser.Sequence nodes)
            |> (fun node -> List.cons node args)
            |> List.rev
        | _ -> failwith "transform_dot_params unreachable case 3"
let%test_unit "transform_dot_args" =
    [%test_eq: Parser.node list]
        (transform_dot_args (-1) [
            Parser.NumberInt 1;
            Parser.NumberInt 2;
        ])
        [
            Parser.NumberInt 1;
            Parser.NumberInt 2;
        ];

    [%test_eq: Parser.node list]
        (transform_dot_args 2 [
            Parser.NumberInt 1;
            Parser.NumberInt 2;
            (* should start accumulating from this *)
            Parser.NumberInt 3; 
            Parser.NumberInt 4;
            Parser.NumberInt 5;
        ])
        [
            Parser.NumberInt 1;
            Parser.NumberInt 2;
            Parser.Sequence [
                Parser.NumberInt 3;
                Parser.NumberInt 4;
                Parser.NumberInt 5;
            ];
        ];

    [%test_eq: Parser.node list]
        (transform_dot_args 1 [
            Parser.NumberInt 1;
        ])
        [
            Parser.NumberInt 1;
            Parser.Sequence [];
        ];
    ()

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
and apply env proc args =
    match proc with
    | Parser.Symbol(expr) ->
        begin
            match expr with
            | "quote" ->
                begin
                    match args with
                    | node :: [] -> node
                    | _ -> failwith ("apply receive invalid params for quote " ^ (Parser.to_string_nodes args true))
                end
            | "cons" ->
                begin
                    match args with
                    | arg_1 :: arg_2 :: [] ->
                        let arg_1_evaluated = eval env arg_1 in
                        let arg_2_evaluated = eval env arg_2 in
                        begin
                            match arg_2_evaluated with
                            | Sequence nodes -> Sequence (arg_1_evaluated :: nodes)
                            | _ -> Pair (arg_1_evaluated, arg_2_evaluated)
                        end
                    | _ -> failwith ("apply receive invalid params for cons " ^ (Parser.to_string_nodes args true))
                end
            | "car" ->
                begin
                    match args with
                    | arg :: [] ->
                        begin
                            let arg_evaluated = eval env arg in
                            match arg_evaluated with
                            | Parser.Pair (node, _) -> node
                            | Parser.Sequence (node :: _) -> node
                            | _ -> failwith ("apply received invalid params for car " ^ (Parser.to_string_nodes args true))
                        end
                    | _ -> failwith ("apply received invalid params for car " ^ (Parser.to_string_nodes args true))
                end
            | "cdr" ->
                begin
                    match args with
                    | arg :: [] ->
                        begin
                            let arg_evaluated = eval env arg in
                            match arg_evaluated with
                            | Parser.Pair (_, node) -> node
                            | Parser.Sequence (_ :: nodes) -> Parser.Sequence nodes
                            | _ -> failwith ("apply received invalid params for cdr " ^ (Parser.to_string_nodes args true))
                        end
                    | _ -> failwith ("apply received invalid params for cdr " ^ (Parser.to_string_nodes args true))
                end
            | "set!" ->
                begin
                    match args with
                    | Parser.Symbol (name) :: node :: [] ->
                        let result = eval env node in
                        Hashtbl.replace env name result;
                        result
                    | _ -> failwith ("apply received invalid params for set!" ^ Parser.to_string_nodes args true)
                end
            | "+" -> Parser.NumberInt(make_operator_handler env (+) args) 
            | "-" -> Parser.NumberInt(make_operator_handler env (-) args) 
            | "*" -> Parser.NumberInt(make_operator_handler env ( * ) args) 
            | "/" -> Parser.NumberInt(make_operator_handler env (/) args)
            | "=" ->
                let first_param = eval env (List.hd args) in
                begin match Seq.find
                    (fun param -> not (compare_nodes (eval env param) first_param))
                    (* skip the first parameter to avoid redundant
                       `eval` *)
                    (List.to_seq args |> Seq.drop 1)
                    with
                        | Some(_) -> Parser.Bool(false)
                        | None -> Parser.Bool(true)
                end
            | _ -> apply env (eval env proc) args
        end
    | Parser.Func(params, body) ->
        (* TODO: optimize by looking at the way `Hashtbl.add` works *)
        let new_env = Hashtbl.copy env in
        let dot_index, params_ = transform_dot_params params in
        let args_evaluated = List.map
            (fun arg -> eval env arg)
            args
        in
        let args_ = transform_dot_args dot_index args_evaluated in
        (* TODO: handle invalid arity case *)
        if List.length params_ != List.length args_
        then failwith
            (Format.sprintf
                "invalid argument count: expected %d; got %d"
                (List.length params_) (List.length args_));
        List.iter2
            (fun param arg ->
                match param with
                | Parser.Symbol(fn_param_sym) ->
                    Hashtbl.add new_env fn_param_sym arg
                | _ -> failwith "apply error")
            params_
            args_;
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
        ", params " ^ Parser.to_string_nodes args true
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

let%test_unit "try_eval_int" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();
    (* TODO: test for failure *)
    [%test_eq: int]
        (try_eval_int env (Parser.NumberInt 3))
        3;
    ()

let%test_unit "make_operator_handler" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();
    [%test_eq: int]
        (make_operator_handler env (+) [NumberInt 1; NumberInt 2])
        (1 + 2);
    [%test_eq: int]
        (make_operator_handler env (-) [NumberInt 1; NumberInt 2])
        (1 - 2);
    [%test_eq: int]
        (make_operator_handler env ( * ) [NumberInt 2; NumberInt 3])
        (2 * 3);
    [%test_eq: int]
        (make_operator_handler env (/) [NumberInt 2; NumberInt 3])
        (2 / 3);
    ()

let%test_unit "apply" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();

    (* test `(quote ...)` *)
    [%test_eq: Parser.node]
        (apply env (Parser.Symbol "quote") [Parser.Symbol "a"])
        (Parser.Symbol "a");
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "quote")
            [Parser.Sequence [
                Parser.Symbol "+";
                Parser.Symbol "a";
                Parser.Symbol "b";
            ]])
        (Parser.Sequence [
            Parser.Symbol "+";
            Parser.Symbol "a";
            Parser.Symbol "b";
            ]);

    (* test `(cons ...)` *)
    [%test_eq: Parser.node]
        (apply env (Parser.Symbol "cons") [Parser.NumberInt 1; Parser.NumberInt 2])
        (Parser.Pair (Parser.NumberInt 1, Parser.NumberInt 2));

    (* test `(car ...)` *)
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "car")
            [Parser.Quote (Parser.Pair (Parser.NumberInt 1, Parser.NumberInt 2))])
        (Parser.NumberInt 1);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "car")
            [Parser.Sequence [
                Parser.Symbol "cons";
                Parser.NumberInt 1;
                Parser.NumberInt 2;
            ]])
        (Parser.NumberInt 1);

    (* test `(cdr ...)` *)
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "cdr")
            [Parser.Quote (Parser.Pair (Parser.NumberInt 1, Parser.NumberInt 2))])
        (Parser.NumberInt 2);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "cdr")
            [Parser.Sequence [
                Parser.Symbol "cons";
                Parser.NumberInt 1;
                Parser.NumberInt 2;
            ]])
        (Parser.NumberInt 2);

    (* test `(set! ...)` *)
    let test_set_env = Hashtbl.create 1 in
    [%test_eq: Parser.node]
        (apply
            test_set_env
            (Parser.Symbol "set!")
            [Parser.Symbol "x"; Parser.NumberInt 1])
        (Parser.NumberInt 1);
    [%test_eq: Parser.node]
        (Hashtbl.find test_set_env "x")
        (Parser.NumberInt 1);

    (* test `+`, `-`, `*`, and `/` *)
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "+")
            [
                Parser.NumberInt 1;
                Parser.NumberInt 2;
                Parser.NumberInt 3;
            ])
        (Parser.NumberInt 6);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "-")
            [
                Parser.NumberInt 1;
                Parser.NumberInt 2;
                Parser.NumberInt 3;
            ])
        (Parser.NumberInt (-4));
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "*")
            [
                Parser.NumberInt 1;
                Parser.NumberInt 2;
                Parser.NumberInt 3;
                Parser.NumberInt 4;
            ])
        (Parser.NumberInt 24);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "/")
            [
                Parser.NumberInt 2;
                Parser.NumberInt 2;
            ])
        (Parser.NumberInt 1);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "/")
            [
                Parser.NumberInt 2;
                Parser.NumberInt 3;
            ])
        (Parser.NumberInt 0);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "/")
            [
                Parser.NumberInt 4;
                Parser.NumberInt 2;
            ])
        (Parser.NumberInt 2);

    (* test `(= ...)` *)
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "=")
            [
                Parser.NumberInt 4;
                Parser.NumberInt 2;
            ])
        (Parser.Bool false);
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Symbol "=")
            [
                Parser.NumberInt 3;
                Parser.NumberInt 3;
                Parser.NumberInt 3;
            ])
        (Parser.Bool true);

    (* test applying function `((lambda ...) ...)` *)
    [%test_eq: Parser.node]
        (apply
            env
            (Parser.Func (
                [Parser.Symbol "x"],
                [Parser.Sequence [
                    Parser.Symbol "+";
                    Parser.Symbol "x";
                    Parser.NumberInt 1;
                ]])
            )
            [
                Parser.NumberInt 2;
            ])
        (Parser.NumberInt 3);

    ()

let%test_unit "eval" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();

    (* test self evaluated node *)
    [%test_eq: Parser.node]
        (eval env (Parser.NumberInt 3))
        (Parser.NumberInt 3);
    [%test_eq: Parser.node]
        (eval env (Parser.String_ "abc"))
        (Parser.String_ "abc");

    (* test let expression *)
    [%test_eq: Parser.node]
        (eval env
            (Parser.Let
                ([Parser.Sequence [Parser.Symbol "x"; Parser.NumberInt 1]],
                [Parser.Symbol "x"])))
        (Parser.NumberInt 1);

    (* test if expression *)
    [%test_eq: Parser.node]
        (eval env
            (Parser.If (Parser.Bool true, Parser.NumberInt 1, Parser.NumberInt 2)))
        (Parser.NumberInt 1);

    (* test symbol *)
    let test_eval_symbol_env = Hashtbl.create 1 in
    let _ = apply
        test_eval_symbol_env
        (Parser.Symbol "set!")
        [Parser.Symbol "x"; Parser.NumberInt 1]
    in
    [%test_eq: Parser.node]
        (eval
            test_eval_symbol_env
            (Parser.Symbol "x"))
        (Parser.NumberInt 1);

    (* test quote *)
    [%test_eq: Parser.node]
        (eval
            env
            (Parser.Quote (Parser.Sequence [Parser.Symbol "+"; Parser.Symbol "x"; Parser.NumberInt 1])))
        (Parser.Sequence [Parser.Symbol "+"; Parser.Symbol "x"; Parser.NumberInt 1]);

    (* test sequence *)
    [%test_eq: Parser.node]
        (eval
            env
            (Parser.Sequence [Parser.Symbol "+"; Parser.NumberInt 1; Parser.NumberInt 1]))
        (Parser.NumberInt 2);

    (* test define *)
    (* TODO: test (define (f x) ...) *)
    let test_eval_define_env = Hashtbl.create 1 in
    [%test_eq: Parser.node]
        (eval
            test_eval_define_env
            (Parser.Define ("x", Parser.NumberInt 1)))
        (Parser.NumberInt 1);

    ()

let eval_nodes env nodes =
    List.map
        (fun node -> eval env node)
        nodes

let%test_unit "eval_nodes" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();
    [%test_eq: Parser.node list]
        (eval_nodes
            env
            [])
        [];

    [%test_eq: Parser.node list]
        (eval_nodes
            env
            [
                Parser.NumberInt 1;
                Parser.NumberInt 2;
            ])
        [
            Parser.NumberInt 1;
            Parser.NumberInt 2;
        ];

    [%test_eq: Parser.node list]
        (eval_nodes
            env
            [
                Parser.Sequence [
                    Parser.Symbol "+";
                    Parser.NumberInt 1;
                    Parser.NumberInt 2;
                ];
                Parser.Sequence [
                    Parser.Symbol "+";
                    Parser.NumberInt 3;
                    Parser.NumberInt 4;
                ];
            ])
        [
            Parser.NumberInt 3;
            Parser.NumberInt 7;
        ];

    ()

(* TODO: consider if the return type should also be a string,
         and should we somehow find a way not to "leak" `env` out *)
let eval_str env input =
    Tokenizer.tokenize input
    |> Parser.parse
    |> eval_nodes env
    |> List.map Parser.to_string

let%test_unit "eval_str" =
    let env : (string, Parser.node) Hashtbl.t = Hashtbl.create 0 in ();
    [%test_eq: string list]
        (eval_str
            env
            "")
        [];

    [%test_eq: string list]
        (eval_str
            env
            "1 2 3")
        [
            "1";
            "2";
            "3";
        ];

    [%test_eq: string list]
        (eval_str
            env
            "(+ 1 2)")
        [
            "3";
        ];

    let env_1 = Hashtbl.create 1 in
    let _ = eval_str env_1 "(define (f x) (+ x 3))" in
    [%test_eq: string list]
        (eval_str
            env_1
            "(f 3)")
        [
            "6";
        ];

    let env_2 = Hashtbl.create 1 in
    let _ = eval_str
        env_2
        "(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))"
    in
    [%test_eq: string list]
        (eval_str
            env_2
            "(fact 5)")
        [
            "120";
        ];

    ()

