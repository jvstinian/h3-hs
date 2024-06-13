# h3-hs

This library provides Haskell bindings to the [H3 API](https://h3geo.org/docs/api/indexing).  

Please note the following when using the functions in this package.  
* The functions call the C methods with little processing of input or output values. 
* The methods taking the `LatLng` type expect latitude and longitude in radians, contrary to 
  what users familiar with the bindings in other languages, e.g. python, might expect. 
* For C methods that return `H3Error` and use output arguments, we return values in the `Either H3ErrorCodes` 
  monad, where `H3ErrorCodes` is a data type and `Enum` instance representing the range of values of `H3Error`.

