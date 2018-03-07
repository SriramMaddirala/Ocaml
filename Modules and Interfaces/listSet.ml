type 'a set = 'a list
let emptyset = []
let is_empty s = match s with [] -> true | _ -> false
let mem x s = List.mem x s
let add x s = if (mem x s) then s else x::s
let subset s1 s2 = List.for_all (fun x -> mem x s2) s1
let eq s1 s2 = (subset s1 s2) && (subset s2 s1)
let union s1 s2 = List.fold_left (fun acc x -> if (mem x acc) then acc else (x::acc)) s1 s2
let intersect s1 s2 = List.fold_left (fun acc x -> if (mem x s2) then (x::acc) else acc) [] s1
let rec check_set s = match s with
| [] -> true
| h::t -> (not (mem h t)) && (check_set t)
let fold f acc (s: 'a set) = List.fold_left f acc s
