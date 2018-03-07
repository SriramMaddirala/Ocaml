type 'a set
val emptyset : 'a set
val is_empty : 'a set -> bool
val mem : 'a -> 'a set -> bool
val add : 'a -> 'a set -> 'a set
val union : 'a set -> 'a set -> 'a set
val intersect : 'a set -> 'a set -> 'a set
val subset : 'a set -> 'a set -> bool
val eq : 'a set -> 'a set -> bool
