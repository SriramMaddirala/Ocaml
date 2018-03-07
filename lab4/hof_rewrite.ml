(* rewrite each of these function definitions as higher-order functions using the rules in lab4.md *)

let rec append l1 l2 = match l1 with
  | [] -> l2
  | (h::t) -> h::(append t l2)

let rec take_until lst s = match lst with
  | [] -> []
  | (h::t) -> if h = s then [] else h::(take_until t s)

(* uncomment and fill in:  *)
 let rec append_hof = fun l1 ->  match l1 with
 | [] -> fun l2-> l2 
 | (h::t) -> let append1_hof = append_hof t in fun l2 -> h::( append1_hof l2 )
 

 let rec take_until_hof = fun lst ->  match lst with
| [] -> fun s-> []  
| (h::t) -> let take_until1_hof = take_until_hof t in fun s-> if h = s then [] else h::(take_until1_hof  s)
