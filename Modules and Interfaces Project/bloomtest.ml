open Bloom
module BloomSparseInt = BloomFilter (SparseSet) (IntHash)
module BloomBoolInt = BloomFilter (BoolSet) (IntHash)
let rec create i lst = match i with
		| 0 -> lst
		| n -> create (n-1) (Random.int (1 lsl 30 -1)::lst)
let insert_ilist = create 200 []
let rec createSparse acc lst = match lst with
| []-> acc
| hd::tl -> createSparse (BloomSparseInt.add hd acc) tl
let create_sparseInt lst = createSparse BloomSparseInt.empty lst

let sparseInt_t, bloomSparseInt = let t1 = Sys.time () in let x = create_sparseInt insert_ilist in
				  let t2 = Sys.time () in t2 -. t1, x

let rec createBool acc lst = match lst with
| []-> acc
| hd::tl -> createBool (BloomBoolInt.add hd acc) tl
let create_boolInt lst = createBool BloomBoolInt.empty lst

let boolInt_t, bloomBoolInt = let t1 = Sys.time () in let y = create_boolInt insert_ilist in
				  let t2 = Sys.time () in t2 -. t1, y

let test_ilist = create 10000000 []
let test_sparseInt lst bloom=
    let rec h1 r lst = match lst with
		| [] -> r
		| h::t -> if (BloomSparseInt.mem h bloom) then h1 (r+1) t else h1 r t
	in h1 0 lst

let sparseInt_test_t, false_SparseInt = let t1 = Sys.time () in let x = test_sparseInt test_ilist bloomSparseInt in
				  let t2 = Sys.time () in t2 -. t1, x

let test_boolInt lst bloom=
    let rec h1 r lst = match lst with
		| [] -> r
		| h::t -> if (BloomBoolInt.mem h bloom) then h1 (r+1) t else h1 r t
	in h1 0 lst

let boolInt_test_t, false_BoolInt = let t1 = Sys.time () in let x = test_boolInt test_ilist bloomBoolInt in
				  let t2 = Sys.time () in t2 -. t1, x

module BloomSparseString = BloomFilter (SparseSet) (StringHash)
module BloomBoolString = BloomFilter (BoolSet) (StringHash)

let insert_slist = 
	let in_file = open_in "top-2k.txt" in
    let rec loop acc i =
        let next_line = try Some (input_line in_file) with End_of_file -> None in
        match (next_line,i) with
			| (_,0) -> acc
            | (Some l,i) -> loop (l::acc) (i-1)
            | (None,i) -> acc
        in
    let lines = try List.rev (loop [] 2048) with _ -> [] in
    let () = close_in in_file in
    lines
    
let test_slist = 
	let in_file = open_in "top-1m.txt" in
    let rec loop acc i =
        let next_line = try Some (input_line in_file) with End_of_file -> None in
        match (next_line,i) with
			| (_,0) -> acc
            | (Some l,i) -> loop (l::acc) (i-1)
            | (None,i) -> acc
        in
    let lines = try List.rev (loop [] (1000000-2048)) with _ -> [] in
    let () = close_in in_file in
    lines
    
let create_sparseString lst =
    let rec h1 r lst = match lst with
		| [] -> r
		| h::t -> h1 (BloomSparseString.add h r) t
	in h1 BloomSparseString.empty lst

let sparseString_t, bloomSparseString = let t1 = Sys.time () in let x = create_sparseString insert_slist in
				  let t2 = Sys.time () in t2 -. t1, x

let create_boolString lst =
    let rec h2 r lst = match lst with
		| [] -> r
		| h::t -> h2 (BloomBoolString.add h r) t
	in h2 BloomBoolString.empty lst

let boolString_t, bloomBoolString = let t1 = Sys.time () in let y = create_boolString insert_slist in
				  let t2 = Sys.time () in t2 -. t1, y

let test_sparseString lst bloom=
    let rec h1 r lst = match lst with
		| [] -> r
		| h::t -> if (BloomSparseString.mem h bloom) then h1 (r+1) t else h1 r t
	in h1 0 lst

let sparseString_test_t, false_SparseString = let t1 = Sys.time () in let x = test_sparseString test_slist bloomSparseString in
				  let t2 = Sys.time () in t2 -. t1, x

let test_boolString lst bloom=
    let rec h1 r lst = match lst with
		| [] -> r
		| h::t -> if (BloomBoolString.mem h bloom) then h1 (r+1) t else h1 r t
	in h1 0 lst

let boolString_test_t, false_BoolString = let t1 = Sys.time () in let x = test_boolString test_slist bloomBoolString in
				  let t2 = Sys.time () in t2 -. t1, x;;

Printf.printf "SparseInt	: build time =	%fs test time = %fs false positives = %d\n" sparseInt_t     sparseInt_test_t     false_SparseInt
Printf.printf "BoolInt		: build time =	%fs test time = %fs false positives = %d\n" boolInt_t        boolInt_test_t        false_BoolInt
Printf.printf "SparseString	: build time =	%fs test time = %fs false positives = %d\n" sparseString_t  sparseString_test_t  false_SparseString
Printf.printf "BoolString	: build time =	%fs test time = %fs false positives = %d\n" boolString_t     boolString_test_t 
