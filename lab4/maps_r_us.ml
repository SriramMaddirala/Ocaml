(* picky function to truncate to places decimals *)
let float_trunc places s =
  let mul = 10. ** (float_of_int places) in
  (floor (s *. mul)) /. mul

let to_meters l = match l with
| [] -> []
| (a,b)::t -> List.map (fun (a,b) -> float_trunc (2) (a *. (.3048) +. (.3048) *. (b/12) )) l

let rot13 s = match s with
| [] -> []
| h::t -> String.map (fun x -> match x with
			|'A'..'M' -> int_to_char( (char_to_int x)+13)  
			|'N'..'Z'-> int_to_char( (char_to_int x)-13)
			|'a'..'m'-> int_to_char( (char_to_int x)+13)
			|'n'..'z'-> int_to_char( (char_to_int x)-13)) s

let wite_out ls i = match ls with 
| [] -> []
| h::t -> String.map (fun x ->  x.charAt(i)= "" ) ls 
