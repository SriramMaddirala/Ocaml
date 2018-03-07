(* Recursion, tail recursion, nested functions.  Your definitions of unzip, list_product, and list_deriv go here. *)

let rec unzip ls = match ls with
| (a,b)::tl -> let x,y = unzip tl in (a::x,b::y)
|[] -> ([],[]) (*The type of unzip should be a list of tuples as that is what unzip returns*)
let rec list_cat (ls : string list)  = match ls with
|a::[] -> a
|[]->""
|a::b -> a^list_cat b  ;; 
let rec list_deriv (xs : int list) : int list = match xs with 
|a::[] -> []
|a::b::t -> (b-a)::list_deriv b::t
|[] -> []
