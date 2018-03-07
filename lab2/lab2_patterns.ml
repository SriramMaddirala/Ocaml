(* Pattern matching examples: try to predict what the result of each expression below will be.
   You can use utop to check. *)

(* tuples *)
let (a,b) = (3,4) in a*b (*a,b is 12*)
let c,d = 1,2(*c,d will be 1,2*)

(* list patterns *)
let (h::t) = [1;2;3](*h is [1] t is [2,3]*)

(*let (x::y::z) = [1] it's failure because you need atleast one more value in the list on the right of the equals sign*)
let (_::rest) = [1;2](*rest will be 2*)

(* "as" patterns *)
let ((a1,b1) as c1) = (2,3) (*a1 will be 2, b1 will be 3 and c1 will be (2,3)*)
let ((a2,b2) as c2, d2) = ((2,3),4) (*a1 will be 2, b1 will be 3 and c1 will be (2,3) and d2 will be 4*)

(* OR patterns *)

(* This or pattern works... *)
let rec make_pairs = function
  | ([] | _::[]) -> []
  | a::b::t -> (a,b) :: make_pairs t
(* It takes a list and turns the element into tuples*)
(* but this one doesn't.  Why?  Fix it.*)
let rec singleton_or_empty_list = function
  | [] -> true | h::[] -> true
  | _ -> false
(*This doesn't work because it needs to have the same amount of identifiers on the left side as on the right. It returns true if it's an empty list or a single element and false otherwise.*)
(* This pattern won't work, due to the *linearity* restriction.  It can be
fixed with "pattern guards" as in Hickey, though that's overkill here. *)
let twins p = match p with
  | (s,t) when s=t -> true
  | (s,t) -> false
(*it returns true if the tuple elements are identical and false otherwise*)
