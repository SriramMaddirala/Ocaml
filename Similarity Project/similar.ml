open SimUtil

(* Your code goes here: *)

(* Define the function that filters out non-alphabetic characters *)
let filter_helper s = match s with 
| 'A'..'Z' -> s
| 'a'..'z' -> s
| _ -> ' '
let filter_chars s = String.map filter_helper s 

(* Define the function that converts a string into a list of words *)
let words s = List.filter (fun x -> not(String.contains x ' ') ) (split_words (filter_chars s))
(* Define the function that converts a list of strings into a list of word lists*)
let wordlists ls = List.map words ls

(* Use Stemmer.stem to convert a list of words into a list of stems *)
let stemlist ws = List.map Stemmer.stem ws

(* Define a function to convert a list into a set *)
(*Given a list of stems return list of stems without duplicates non-recursively*)
let to_set lst = List.rev(List.fold_left (fun acc x-> if(List.mem x acc) then acc else x::acc) []  lst)
(* Define the similarity function between two sets: size of intersection / size of union *)
let intersection_size s1 s2 = List.length(List.fold_left(fun acc x  -> if(List.mem x s2) then x::acc else acc) [] s1 )
let union_size s1 s2 = List.length(to_set (s1@s2))
let similarity s1 s2 =(float (intersection_size s1 s2)) /. (float (union_size s1 s2))

(* Find the most similar representative file *)
let find_max repsims repnames = List.fold_left (fun acc x -> max acc x) (0.,"") (List.combine repsims repnames) 

let main all replist_name target_name =
  (* Read the list of representative text files *)
  let repfile_list = file_lines replist_name in
  (* Get the contents of the repfiles and the target file as strings *)
  let rep_contents = List.map file_as_string repfile_list in
  let target_contents = file_as_string target_name in
  (* Compute the list of words from each representative *)
  let rep_words = wordlists rep_contents in
  (* Convert the target text file into a list of words *)
  let target_words = words target_contents in
  (* Compute the lists of stems in all rep files and the target file *)
  let rep_stemlists = List.map (fun y -> (List.map stemlist y)) [rep_words] in
  let target_stemlist = List.map stemlist [target_words] in
  (* Convert all of the stem lists into stem sets *)
  let rep_stemsets = List.map (fun y-> List.map to_set y) rep_stemlists in
  let target_stemset = List.map to_set target_stemlist in
  (* Compute the similarities of each rep set with the target set *)
  let repsims = List.map (fun x -> similarity target_stemset x) rep_stemsets in
  let (sim,best_rep) = find_max repsims repfile_list in
  let () = if all then
  (* print out similarities to all representative files *)
  let () = print_endline "File\tSimilarity" in
   List.iter2 (fun x y -> print_endline ((string_of_float x)^"\t"^y)) repsims repfile_list else begin
  (* Print out the winner and similarity *)
  let () = print_string "The most similar file to " in 
  let () = print_string target_name in
  let () = print_string  " was " in
  let () = print_endline best_rep in
  let () = print_string "Similarity: "in
  print_endline (string_of_float sim)  end in
  (* this last line just makes sure the output prints before the program exits *)
  flush stdout
