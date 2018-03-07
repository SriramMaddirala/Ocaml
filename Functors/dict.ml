module type EltSig = sig
  type k
  type v
  val eq : k -> k -> bool
end

module type DictSig = sig
  type t
  type k
  type v
  val empty : t
  val add : k -> v -> t -> t
  val lookup : k -> t -> v
end

(* Add ListDict Functor here *)
module EqListDict( Elt : EltSig) 
: DictSig with type k = Elt.k
with type v = Elt.v
= struct
  type k = Elt.k
  type v = Elt.v
  type t = (k * v) list 
  let empty = []
  let add k v d = (k,v)::d
  let rec lookup k d = match d with
  | [] -> raise (Not_found)
  | (a,b)::tl -> if(Elt.eq a k) then b else lookup k tl 
 end

(* Add DefaultElt signature here *)
module type DefaultEltSig = sig 
include EltSig
val default : v 
end
module type DefaultDictSig = sig
  type t
  type k
  type v
  val default : v
  val empty : t
  val add : k -> v -> t -> t
  val lookup : k -> t -> v
end
(* DefaultDict functor: *)
module DefaultDict(DefaultElt : DefaultEltSig) : DefaultDictSig with type k = DefaultElt.k
with type v = DefaultElt.v
= struct 
  type k = DefaultElt.k
  type v = DefaultElt.v
  let default = DefaultElt.default
  type t = (k * v) list
  let empty = []
  let add k v d = (k,v)::d
  let rec lookup k d = match d with
  | [] -> default
  | (a,b)::tl -> if(DefaultElt.eq a k) then b else lookup k tl
end

(* DefaultDictFunctor: *)
module DefaultDictFunctor = struct end
