open Parser

module Evaluator =
    struct
        let eval node =
            match node with
            | Parser.NumberInt(value) -> Parser.NumberInt(value)
            | Parser.NumberFloat(value) -> Parser.NumberFloat(value)
            | Parser.String_(value) -> Parser.String_(value)
            (* | Parser.Symbol(expr) -> Hashtbl.find env expr *)
            | _ -> failwith "eval node not implemented"
    end

