1. `module A`
> Not Legal
>> The value `x` has mismatched types in the signature and struct
2. `module B`
> Legal
>> module B : sig val b : bytes end
3. `module c`
> Not Legal
>> syntax error
4. `module D`
> Legal
>> module D : sig val f : int -> int val g : int list -> int list end
5. `let module E = ...`
> Legal
>> int = 1
6. `let module F : sig ...`
> Not Legal
>> Error: This expression has type F.t -> F.t but an expression was expected of type F.t -> F.t The type constructor F.t would escape its scope 
7. `module type GaSig`
> Legal
>> module type GaSig = sig type t end;  module type GbSig = sig val x : int end; module Ga : sig type t = int end; module Gb : GbSig module Gc : sig type t val x : t end
8. `module type HSig`
> Not Legal
>> Error: Syntax error 
