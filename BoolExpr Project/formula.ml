(* formula.ml - command line driver for hw2 boolExpr *)
let arglist = Array.to_list Sys.argv in match arglist with
| _::"--eval"::t -> BoolExpr.main t
| _::t -> BoolExpr.main t
| _::"--findstat"::t -> BoolExpr.sat_main () 
| _ -> failwith "there's always at least one argument."
