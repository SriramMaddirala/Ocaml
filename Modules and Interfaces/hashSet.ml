type 'a set = 'a list array

let hs x = (Hashtbl.hash x) mod 65537

let create () = Array.make 65537 []
let emptyset = create ()
let is_empty s = Array.fold_left (fun e ai -> e && (ai=[])) true s

let mem x s = List.mem x s.(hs x)

let add_update x s =
  let h = hs x in
  if List.mem x s.(h) then s else (s.(h) <- x::s.(h) ; s)

let add x s =
  let s' = Array.copy s in add_update x s'

let union u v =
  Array.mapi (fun i ui -> ui @ v.(i)) u

let list_intersect l1 l2 =
  List.fold_left (fun acc x -> if (List.mem x l2) then (x::acc) else acc) [] l1

let intersect u v =
  Array.mapi (fun i ui -> list_intersect ui v.(i)) u

let list_subset s1 s2 =
  List.for_all (fun x -> List.mem x s2) s1

let subset u v =
  let si = Array.mapi (fun i ui -> list_subset ui v.(i)) u in
  Array.fold_left (fun e si -> e && si) true si

let eq u v = (subset u v) && (subset v u)
