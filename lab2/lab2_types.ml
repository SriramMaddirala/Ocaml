(* Type inference examples.  These functions are "not polymorphic enough"  *)
(* Don't add or remove lines from this file, it will break gitbot *)

(* Intended type of pairwith: list of tuples
   Actual type: list of tuples
   Explanation:'a -> 'b list -> ('a * 'b) list = <fun>
 *)
let rec pairwith x lst =
  match lst with
  | [] -> []
  | (h::t) -> (x,h) :: pairwith x t


(* Intended type of has_any:boolean
   Actual type:Error
   Explanation: This expression has type 'a list but an expression was expected of type 'a
 *)
let rec has_any x lst =
  match lst with
  | [] -> false
  | (h::t) -> x=h 

(* Intended type of lookup:String
   Actual type: list of a type
   Explanation:'a -> ('a * bytes) list -> bytes = <fun>
 *)
let rec lookup key lst =
  match lst with
  | [] -> "No match"
  | (k,v)::t -> if k=key then v else lookup key t


(* Intended type of reverse :list 
   Actual type: error
   Explanation:Unbound value tail_rev as no rec   
 *)
let rec tail_rev acc ls =
  match ls with
  | [] -> acc
  | (h::t) -> tail_rev (h::acc) t
let reverse = tail_rev []
