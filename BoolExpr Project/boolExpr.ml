(* read std input to eof, return list of lines *)
let read_lines () : string list =
  let rec read_help acc =
    try read_help ((read_line ())::acc) with End_of_file -> List.rev acc
  in read_help []

(* split a string at word boundaries and parens *)
let wordlist s : string list =
  let splitlist = Str.full_split (Str.regexp "\\b\\|(\\|)") s in
  let rec filter_splist lst = match lst with
    | [] -> []
    | (Str.Delim "(")::t -> "(" :: (filter_splist t)
    | (Str.Delim ")")::t -> ")" :: (filter_splist t)
    | (Str.Delim _) :: t -> filter_splist t
    | (Str.Text s) :: t -> let s' = String.trim s in
			   let t' = (filter_splist t) in
			   if not (s' = "") then s' :: t' else t'
  in filter_splist splitlist

(* is s a legal variable name? *)
let is_varname s =
  let rec checker i =
    if i = 0 then
      'a' <= s.[i] && s.[i] <= 'z'
    else
      (('a' <= s.[i] && s.[i] <= 'z') ||
  	   ('0' <= s.[i] && s.[i] <= '9')) && checker (i-1)
  in checker ((String.length s) - 1)

(* tokens - you need to add some here *)
(*added tokens*)
type bexp_token = OP | CP | NAND | OR | XOR | EQ | NOT | AND  | CONST of bool | VAR of string

(* convert a string into a token *)
let token_of_string = function
  | "(" -> OP
  | ")" -> CP
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "xor" -> XOR
  | "=" -> EQ
  | "nand" -> NAND
  | "T" -> CONST true
  | "F" -> CONST false
  | s -> if (is_varname s) then (VAR s) else (invalid_arg ("Unknown Token: "^s))

(* convert a list of strings into a a list of tokens *)
let tokens wl : bexp_token list = List.map token_of_string wl

(* type representing a boolean expression, you need to add variants here *)
(*added variants*)
type boolExpr = Const of bool
| Id of string
|  Nand of boolExpr * boolExpr
| And of boolExpr * boolExpr
| Or of boolExpr * boolExpr
| Xor of boolExpr * boolExpr
| Eq of boolExpr * boolExpr
| Not of boolExpr
(* attempt to turn a list of tokens into a boolean expression tree.
A token list representing a boolean expression is either
 + a CONST token :: <more tokens> or
 + a VAR token :: <more tokens> or
 + an OPEN PAREN token :: a NAND token :: <a token list representing a boolean expression> @
                                          <a token list representing a boolen expression> @ a CLOSE PAREN token :: <more tokens>
 any other list is syntactically incorrect. *)
let parse_bool_exp tok_list =
(* when possibly parsing a sub-expression, return the first legal expression read
   and the list of remaining tokens  *)
(*added parsing mechanisms for the rest of the operators*)
  let rec parser tlist = match tlist with
    | (CONST b)::t -> (Const b, t)
    | (VAR s)::t -> (Id s, t)
    | OP::NAND::t -> let (a1, t1) = parser t in
                    let (a2, t2) = parser t1 in
                    (match t2 with
                     | CP::t' -> ((Nand (a1,a2)), t')
		                 | _ -> invalid_arg "sexp: missing )")
    | OP::AND::t -> let (a1, t1) = parser t in
                    let (a2, t2) = parser t1 in
                    (match t2 with
                     | CP::t' -> ((And (a1,a2)), t')
                                 | _ -> invalid_arg "sexp: missing )")		
    | OP::OR::t -> let (a1,t1) = parser t in
			let (a2, t2) = parser t1 in
                         (match t2 with
                         |CP::t' -> ((Or (a1, a2)), t')
                                    |_-> invalid_arg "sexp: missing )")
    | OP::XOR::t -> let (a1,t1) = parser t in
                        let (a2, t2) = parser t1 in
                         (match t2 with
                         |CP::t' -> ((Xor (a1, a2)), t')
                                    |_-> invalid_arg "sexp: missing )")
    | OP::EQ::t -> let (a1,t1) = parser t in
                        let (a2, t2) = parser t1 in
                         (match t2 with
                         |CP::t' -> ((Eq (a1, a2)), t')
                                    |_-> invalid_arg "sexp: missing )")
    | OP::NOT::t -> let (a1,t1) = parser t in
                         (match t1 with
                         |CP::t' -> (Not (a1), t')
                                    |_-> invalid_arg "sexp: missing )")   
 | _ -> invalid_arg "parse failed."
  in let bx, t = parser tok_list in
     match t with
     | [] -> bx 
     | _ -> invalid_arg "parse failed: extra tokens in input."

(* pipeline from s-expression string to boolExpr *)
let bool_exp_of_s_exp s = s |> wordlist |> tokens |> parse_bool_exp

(* evaluate the boolean expression bexp, assuming the variable names
   in the list tru are true, and variables not in the list are false *)
(*Adding mechanisms for evaluating other boolean operators*)
let rec eval_bool_exp bexp tru =
  match bexp with
  | Const b -> b
  | Id s -> List.mem s tru
  | Nand (x1, x2) -> not ((eval_bool_exp x1 tru) && (eval_bool_exp x2 tru))
  | And (x1, x2) -> ((eval_bool_exp x1 tru) && (eval_bool_exp x2 tru))
  | Or (x1, x2) -> ((eval_bool_exp x1 tru) || (eval_bool_exp x2 tru))
  | Xor (x1, x2) -> (((eval_bool_exp x1 tru) || (eval_bool_exp x2 tru)) && (( not (eval_bool_exp x1 tru)) || ( not (eval_bool_exp x2 tru))))
  | Not (x1) -> not ( eval_bool_exp x1 tru)
  | Eq (x1, x2) -> ((eval_bool_exp x1 tru) == (eval_bool_exp x2 tru))
(* You need to add some new stuff in here: *)

(* list all the subsets of the list s *)
(*recursively get all subsets with base-case being empty list and recursive case being a list from h::tl use map to apply the subset building function through every item in the subsets tl *)
let rec subsets s = match s with 
| [] -> [[]]
| h :: tl -> let s = subsets tl in s @ (List.map (fun x -> h:: x) s)
(* return true if variable s is already in the list of variables and false otherwis; it is tail-recursive*)
let rec addvar_checkduplicate acc s = match acc with
| [] ->  true
| h::t -> if(compare h s == 0) then (false) else addvar_checkduplicate t s
(*tail-recursive helper function that uses the checker parses through the expression to find variable names and checks using the previous checker function before adding it to the list of variables*)
let rec var_helper bexp acc = match bexp with
| Id s -> if(addvar_checkduplicate acc s) then  (acc @ [s]) else acc
| Const b -> acc
| And (x1, x2) -> var_helper x1 (var_helper x2 acc)
| Or (x1, x2) -> var_helper x1 (var_helper x2 acc)
| Xor (x1, x2) -> var_helper x1 (var_helper x2 acc)
| Eq (x1, x2) -> var_helper x1 (var_helper x2 acc)
| Not (x1) -> var_helper x1 acc
| Nand (x1, x2) -> var_helper x1 (var_helper x2 acc)
(* find all the variable names in a boolExpr *)
(*used helper function*)
let var_list bexp = var_helper (bexp) []
(*find_sat_helper takes in the boolean expression, the powerset of variables in the expression, a result variable that makes it tail-recursive and an index. This function iterates from end to start through the list of subsets (total) using the index which is passed in as the length-1 and checks if the given subset of variables makes the expression true or not. It ends when index=0 case is accounted for and keeps track of the smalles possible subset by checking the length of the subset being evaluated with "smallest" and the shorter of the two is passed in as smallest in the recursive call and smallest is returned in the end*)
let rec find_sat_helper bexp total smallest index = match (eval_bool_exp bexp (List.nth total (index - 1))) with
| false -> if(index == 1) then smallest else find_sat_helper bexp total smallest (index-1)
| true -> match smallest with 
	| [] -> find_sat_helper bexp total (List.nth total (index-1)) (index-1)
	| h::t ->  if ((List.length (List.nth total (index -1))) < List.length smallest) 
			then  find_sat_helper bexp total (List.nth total (index-1)) (index-1) else find_sat_helper bexp total smallest (index-1) 
(* find a list of variables that when set to true make the expression true *)
(* Checks to make sure that the expression evaluates to true. Otherwise just returns None. If it evaluates to true thenit calls find_sat_helper to find the smallest possible subset of variables that if true make the expression true*)
let find_sat_set bexp : string list option = match (eval_bool_exp bexp (var_list bexp)) with
| false -> None
| true -> match (eval_bool_exp bexp []) with
		| false ->  Some(find_sat_helper (bexp) (subsets(var_list bexp)) [] (List.length (subsets(var_list bexp))))
		| true -> Some([])
(* Changes a option list type to a string list; needed it to print in sat_main as String.concat only operates on string type*)
let option_to_stringlist opt = match opt with
| None -> []
| Some(h::t) -> h::t
| Some([])-> []
(* fill this in also *)
(* Reads line, parses expression, prints not satisfiable if the expression can't be satisfied and prints the smallest possible subset of variables that would satisfy it if it can be satisfied*)
let sat_main () = 
let sExpr = String.concat " " (read_lines ()) in
let bExpr = bool_exp_of_s_exp sExpr in
let satfind = find_sat_set bExpr in
let output = if (satfind == None) then "Not Satisfiable" else ("Satisfiable when the variables {" ^ (String.concat ", " (option_to_stringlist satfind)) ^"} are set to true.") in 
print_endline (output);;
(* let () = sat_main () *)

let main true_vars_list =
  let sExpr = String.concat " " (read_lines ()) in
  let bExpr = bool_exp_of_s_exp sExpr in
  let result = eval_bool_exp bExpr true_vars_list in
  let svarlist = " when the variables {" ^ (String.concat ", " true_vars_list) ^"} are set to true." in
  let output = (if result then "True" else "False")^svarlist in
  print_endline output
