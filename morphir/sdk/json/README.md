# Docs for Serializing Custom Types of Morphir IR in Scala.
 Note: [Circe](https://circe.github.io/circe/codec.html) Serialization Library was used for serialization

Type annotations can start to get long if you have records with many fields. 
Sometimes we want to give a type a more  descriptive name hence the use of type aliases. 
A [type alias]() is a shorter name for a type. In Morphir IR, there are (as at this writing) two 
categories of types namely [TypeAlias]() and [CustomTypes]().

## CustomTypes
Custom Types as the name suggests are user defined types of any other types with or without 
constructor arguments. These constructors may also have custom or primitive types as required
parameters. Example ::
  ```
  type Errors = 
    ModuleNotFound ModuleName 
    | InvalidType 
  ``` 

In the morphir-IR Json, these types are represented as a array whose first argument is the string 
of "CustomTypeDefinition" followed by its definition within the same array scope.

### Encoding
In order to encode a custom type, ... 


### Decoding
In decoding a custom type, ...


## Type Aliases 


### Known Types 
In encoding or decoding of know types, define a function that takes a generic type to encode or decode 
as the first parameter group and an encoder function that takes of type A and returns a JValue, as the 
second parameter group.The return type of the main encoder function should be a JValue. Within encode type 
function, pattern match on the type and return it's appropriate Json value representation whiles taking 
into mind, [Morphir Json Mappings](https://github.com/finos/morphir-elm/blob/main/docs/json-mapping.md) 
representation.  
* Below is an example showing how to encode Type.ExtensibleRecord  
* This type has a definition of type [Type.ExtensibleRecord]() with 3 args and returns a JArray whose first item is a string equals
"ExtensibleRecord" and the appropriate encoders called on each element of the list.
  ```
  encodeType[A](typ: Type[A]), (encodeA:A => JValue) : JValue {
    type match {
      case Type.ExtensibleRecord(arg1, arg2, arg3) => JArray(
        List(JString("ExtensibleRecord"),
          encodeA(arg1),
          encodeName(arg2),
          encodeList(arg3)(encodeField)
        ))
      ....
    }
  }
  ```

## Encoding / Decoding String
  ### Encoding String
  In order to encode string, define a function that takes a [String]() type parameter and returns the superclass [JValue]() 
  but the actual return is a [JString]() of the parameter passed. 
  
  ```
    encodeString(str:String) : JValue = JString(str)
  ```

  ### Decoding String
  In order to decode string, define a function that takes a parameter of type [JValue](), then pattern match for case of JString
  and return the [String](). 

## Encoding / Decoding List
  A List in its nature can be a list of any data-type. In order to encode or decode a list, we need to pass the exact data-type encoder
  to the list encoder.

  ### Encoding List
  In order to encode a list, define a function that takes a generic parameter of type [List]() as the first group argument and 
  define a function that takes an encoder of the same generic type and returns a [JValue]() as the second group argument.  
  ```
  encodeList[A](list: List[A])(encodeA:A => JValue) : JValue = JArray( )
  ```
  ### Decoding List
  In order to encode hence to encode or decode such data-type, after the call of the encodeList or 
  decodeList serializer, the appropriate data-type encoder should be passed as a second group parameter argument. 
  Example to encode a list of strings -> encodeList(param)(encodeString)