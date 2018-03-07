1. `listSet.ml : sset.mli`
> Legal
>> listSet provides all types and values in the sset signature
2. `hashSet.ml : sset.mli`
> Not Legal
>> The type of this expression, '_a list array,contains type variables that cannot be generalized
3. `funSet.ml : sset.mli`
> Not Legal
>> funSet does not provide all the types and values in the sset signature  
4. `listSet.ml : rset.mli`
> Not Legal
>>  listSet does not provide all types and values in the rset signature
5. `hashSet.ml : rset.mli`
> Not Legal
>> The type of this expression, '_a list array,contains type variables that cannot be generalized
6. `funSet.ml : rset.mli`
> Legal
>>  funSet provides all types and values in the rset signature
7. `listSet.ml : fset.mli`
> Legal
>>  listSet provides all types and values in the fset signature
8. `hashSet.ml : fset.mli`
> Not Legal
>> The type of this expression, '_a list array,contains type variables that cannot be generalized
9. `funSet.ml : fset.mli`
>Not Legal
>>  funSet does not provide all the types and values in the fset signature
