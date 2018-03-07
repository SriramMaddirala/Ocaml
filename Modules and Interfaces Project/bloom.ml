(* Bloom Filter implementation.  This file will not compile as is. *)
module type memset = sig
    type elt (* type of values stored in the set *)
    type t (* abstract type used to represent a set *)
    val mem : elt -> t -> bool
    val empty : t
    val is_empty : t -> bool
    val add : elt -> t -> t
    val from_list : elt list -> t
    val union : t -> t -> t
    val inter : t -> t -> t
  end

(* Define the hashparam signature here *)
module type hashparam = sig
    type t
    val hashes: t -> int list
end
(* Define SparseSet module here, using the Set.Make functor *)
module SparseSet : memset with type elt = int = struct 
include Set.Make(struct type t = int let compare = Pervasives.compare end) 
let from_list lst = let finset = empty in List.fold_left (fun acc a-> add a acc)  finset lst
end
(* Fill in the implementation of the memset signature here.  You'll need to expose the elt type *)
module BoolSet : memset with type elt = int = struct
    type elt = int
    type t = bool array 
    let make_arr_t i = let result = Array.make i false in Array.set result (i-1) true; result
    let rec (&@) s1 s2 = let (short,long) =
      if (Array.length s1 < Array.length s2) then (s1,s2) else (s2,s1) in
      Array.mapi (fun i li -> if i < (Array.length short) then li && short.(i) else li) long
    (* element-wise or of two arrays: *)
    let rec (|@) s1 s2 = let (short,long) =
      if (Array.length s1 < Array.length s2) then (s1,s2) else (s2,s1) in
      Array.mapi (fun i li -> if i < (Array.length short) then li || short.(i) else li) long  
   let empty = [||] 
   let is_empty l = (Array.length l =0)
   let mem i lst = if(is_empty lst) then false else  if(Array.length lst < i) then false else lst.(i-1)
   let add i lst =  lst |@ (make_arr_t i)    
   let from_list lst = let finset = empty in List.fold_left (fun acc a -> add a acc) finset lst
   let union l1 l2 = l1 |@ l2
   let inter l1 l2 = l1 &@ l2
end
(* Fill in the implementation of a BloomFilter, matching the memset signature, here. *)
(* You will need to add some sharing constraints to the signature below. *)
module BloomFilter(S : memset with type elt = int)(H : hashparam) : memset with type elt = H.t = struct
    type elt = H.t
    type t = S.t
    (* Implement the memset signature: *)
    let empty = S.empty
    let is_empty ls = S.is_empty ls
    let from_list lst = S.from_list
    let union l1 l2 = S.union l1 l2
    let inter l1 l2 = S.inter l1 l1
    let rec checkList lst = match lst with
    | []-> true
    | hd::tl -> if(hd=false) then false else checkList tl
    let mem i lst = let hshlst = H.hashes i in checkList (List.map (fun el -> S.mem el lst ) hshlst)
    let add i lst = let hshlst = H.hashes i in List.fold_left (fun acc el -> S.add el acc ) lst hshlst    
    let from_list lst = let finset = empty in List.fold_left (fun acc a -> add a acc) finset lst
end

(* A hashparam module for strings... *)
module StringHash = struct
    type t = string (* I hash values of type string *)
    let hlen = 15
    let mask = (1 lsl hlen) - 1
    let hashes s =
      let rec hlist n h = if n = 0 then [] else (h land mask)::(hlist (n-1) (h lsr hlen)) in
      hlist 4 (Hashtbl.hash s)
  end

(* Add the IntHash module here *)
module IntHash = struct 
 type t = int
let h1 n = ((795 * n) + 962) mod 1031
let h2 n = ((386 * n) + 517) mod 1031
let h3 n = ((937 * n) + 693) mod 1031
let hashes n = [h1 n; h2 n; h3 n]
end
