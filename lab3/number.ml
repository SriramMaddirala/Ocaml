type number = Int of int | Float of float;;

let z1:number = Int 3;;
let z2:number = Int 4;;
let r1:number = Float 2.;;
let r2:number = Float 3.;;


let rec to_int (n : number) : int option  = match n with
|Int x -> Some x
|_ -> None

let rec to_float (n : number) : float option  = match n with
|Float x -> Some x
|_ -> None

let rec float_of_number (n : number) : float  = match n with
|Int x -> (float_of_int x)
|Float y -> y

let rec (+?) x y = match (x,y) with
|( (Int x),(Int y) ) -> Int (x+y)
|( (Float x),(Int y)) -> Float (x +. (float_of_int y) )
|( (Int x), (Float y)) -> Float((float_of_int x) +. y)
| (Float x, Float y) -> Float(x +.y);;
