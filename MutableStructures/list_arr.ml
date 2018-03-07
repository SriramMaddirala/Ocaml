let rec rev_array_helper a rev index = match (index < (Array.length a)) with
| true -> Array.set rev (((Array.length a)- 1 - index)) (a.(index)); rev_array_helper a rev (index+1)
| false -> Array.iteri (fun i x -> Array.set a i rev.(i) ) a 
let revmaker a = if(a == [||]) then [||] else Array.make (Array.length a) (a.(0))
let rev_array a = let reva = revmaker a in (rev_array_helper a reva 0)
let rec zip_array_helper u v index result = match (index < (Array.length u)) with
| true -> result.(index) <- ((u.(index)),(v.(index))); zip_array_helper u v (index + 1) result
| false -> result
let zip_array u v = let result = Array.make (Array.length u) (u.(0),v.(0)) in zip_array_helper u v 0 result
