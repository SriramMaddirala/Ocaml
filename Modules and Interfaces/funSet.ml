(* funset.ml -- function representation of sets *)

(* an 'a set returns true if an element belongs to it *)
type 'a set = 'a -> bool

(* no element belongs to the empty set *)
let emptyset : 'a set = fun _ -> false
(* membership testing is straightforward *)
let mem x (s: 'a set) = s x

(* adding an element: test for x, otherwise return what s says *)
let add x (s: 'a set) : 'a set = fun y -> (y=x) || (s y)

let union s1 s2 = fun x -> (s1 x) || (s2 x)

let intersect s1 s2 = fun x -> (s1 x) && (s2 x)

let rem x s = fun y -> (y <> x) && (s y)

let setminus s1 s2 = fun x -> (s1 x) && (not (s2 x))

let range a b = fun x -> (a <= x) && (x <= b)

(* subset: a is a subset of b if every element of a belongs to b *)
(* but there is no obvious way to enumerate the elements of a here *)
