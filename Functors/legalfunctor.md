1. `module A`
>  Legal
>> The functor A creates a module by not adding any fields, and the declaration doesn't constrain its signature.
2. `module BB`
> Legal
>> Module specifies everything to be used in declaration above it
3. `module c`
> Not Legal
>> c cannot be lower case
4. `module DF` 
> Not Legal
>> Syntax Error
5. `Module EF`
> Legal
>> No problem restricting signature to something declared in itself
6. `Module FF`
> Not legal
>> This expression has type FF.ft -> bool                                                but an expression was expected of type FF.ft -> bool                                  The type constructor FF.ft would escape its scope 
