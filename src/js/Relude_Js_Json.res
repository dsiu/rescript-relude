@ocaml.doc(`
[Relude.Js.Json] contains helper functions for dealing with [Js.Json.t] values.
`)
open Relude_Function.Infix

@ocaml.doc("
Type alias for Js.Json.t.
")
type json = Js.Json.t

@ocaml.doc("
Type alias for Js.Dict.t(Js.Json.t)
")
type dict = Js.Dict.t<json>

@ocaml.doc("
Shows a JSON value with optional space indentation
")
let show = (~indentSpaces: int=2, json: json): string =>
  Js.Json.stringifyWithSpace(json, indentSpaces)

////////////////////////////////////////////////////////////////////////////////
// Test functions
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Checks if the Js.Json.t value is a null
")
let isNull: json => bool = json => Js.Json.test(json, Js.Json.Kind.Null)

@ocaml.doc("
Checks if the Js.Json.t value is a bool
")
let isBool: json => bool = json => Js.Json.test(json, Js.Json.Kind.Boolean)

@ocaml.doc("
Checks if the Js.Json.t value is a string
")
let isString: json => bool = json => Js.Json.test(json, Js.Json.Kind.String)

@ocaml.doc("
Checks if the Js.Json.t value is a number
")
let isNumber: json => bool = json => Js.Json.test(json, Js.Json.Kind.Number)

@ocaml.doc("
Checks if the Js.Json.t value is a JSON object
")
let isObject: json => bool = json => Js.Json.test(json, Js.Json.Kind.Object)

@ocaml.doc("
Checks if the Js.Json.t value is a JSON array
")
let isArray: json => bool = json => Js.Json.test(json, Js.Json.Kind.Array)

////////////////////////////////////////////////////////////////////////////////
// Constructor functions (encode values as Json)
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Creates a Js.Json.t null value
")
let null: json = Js.Json.null

@ocaml.doc("
Creates a Js.Json.t boolean value from a boolean
")
let fromBool: bool => json = Js.Json.boolean

@ocaml.doc("
Creates a Js.Json.t string value from a string
")
let fromString: string => json = Js.Json.string

@ocaml.doc("
Creates a Js.Json.t number value from an int
")
let fromInt: int => json = \">>"(float_of_int, Js.Json.number)

@ocaml.doc("
Creates a Js.Json.t number value from a float
")
let fromFloat: float => json = Js.Json.number

@ocaml.doc("
Creates a Js.Json.t value from an option('a). If the option is [None], a null
value is returned, otherwise, the value is encoded using the given function.
")
let fromOption: ('a => json, option<'a>) => json = (encode, opt) =>
  opt |> Relude_Option.fold(null, encode)

@ocaml.doc("
Creates a Js.Json.t array value from an array of Js.Json.t values
")
let fromArrayOfJson: array<json> => json = Js.Json.array

@ocaml.doc("
Creates a Js.Json.t array value from an array of values that can be converted to Js.Json.t values with the given function
")
let fromArrayOfJsonBy: 'a. ('a => json, array<'a>) => json = (f, items) =>
  Array.map(f, items) |> fromArrayOfJson

@ocaml.doc("
Creates a Js.Json.t array value from an list of Js.Json.t values
")
let fromListOfJson: list<json> => json = \">>"(Relude_List.toArray, fromArrayOfJson)

@ocaml.doc("
Creates a Js.Json.t array value from an list of values that can be converted to
Js.Json.t values with the given function
")
let fromListOfJsonBy: 'a. ('a => json, list<'a>) => json = (f, items) =>
  Relude_List.map(f, items) |> fromListOfJson

@ocaml.doc("
Creates a Js.Json.t object value from a Js.Dict containing Js.Json.t values.
")
let fromDictOfJson: dict => json = Js.Json.object_

@ocaml.doc("
Creates a Js.Json.t array value from an array of Js.Dict.t(Js.Json.t) values.
")
let fromArrayOfDictOfJson: array<dict> => json = Js.Json.objectArray

@ocaml.doc("
Creates a Js.Json.t array value from an list of Js.Dict.t(Js.Json.t) values.
")
let fromListOfDictOfJson: list<dict> => json = \">>"(Relude_List.toArray, Js.Json.objectArray)

@ocaml.doc("
Creates a Js.Json.t object value from an array of key/value (string/Js.Json.t)
tuples.
")
let fromArrayOfKeyValueTuples: array<(Js.Dict.key, json)> => json = tuples =>
  fromDictOfJson(Js.Dict.fromArray(tuples))

@ocaml.doc("
Creates a Js.Json.t object value from an list of key/value (string/Js.Json.t)
tuples.
")
let fromListOfKeyValueTuples: list<(Js.Dict.key, json)> => json = tuples =>
  fromDictOfJson(Js.Dict.fromList(tuples))

////////////////////////////////////////////////////////////////////////////////
// Basic conversions from the top-level Js.Json.t type to the specific Json
// subtypes.  These functions do not unpack nested json objects - i.e. asDict
// will attempt to convert a [Js.Json.t] value to an [option(Js.Dict.t(Js.Json.t))]
// but does not attempt to decode the resulting dictionary.

// Note: there are intentially very basic. For more advanced decoding, try a
// library like https://github.com/mlms13/bs-decode
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Attempts to decode the given Js.Json.t value as a null.  Returns [Some(())] if
it is a [null], otherwise [None].
")
let toNull: json => option<unit> = json => json |> Js.Json.decodeNull |> Relude_Option.void

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [boolean]
")
let toBool: json => option<bool> = Js.Json.decodeBoolean

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [string]
")
let toString: json => option<string> = Js.Json.decodeString

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as an [int]
")
let toInt: json => option<int> = json =>
  json |> Js.Json.decodeNumber |> Relude_Option.map(int_of_float)

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [float]
")
let toFloat: json => option<float> = Js.Json.decodeNumber

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as an array of [Js.Json.t]
values.
")
let toArrayOfJson: json => option<array<json>> = Js.Json.decodeArray

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as an array of [Js.Json.t]
values, with a fallback.
")
let toArrayOfJsonOrElse: (array<json>, json) => array<json> = (default, json) =>
  json |> toArrayOfJson |> Relude_Option.getOrElse(default)

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as an array of [Js.Json.t]
values, with a fallback of an empty array.
")
let toArrayOfJsonOrEmpty = toArrayOfJsonOrElse([])

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a list of [Js.Json.t] values.
")
let toListOfJson: json => option<list<json>> = json =>
  json |> toArrayOfJson |> Relude_Option.map(Relude_Array.toList)

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as an list of [Js.Json.t] values,
with a fallback.
")
let toListOfJsonOrElse = (default, json) => json |> toListOfJson |> Relude_Option.getOrElse(default)

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a list of [Js.Json.t] values,
with a fallback of an empty list.
")
let toListOrEmpty = toListOfJsonOrElse(list{})

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [Js.Dict.t(Js.Json.t)]
")
let toDictOfJson: json => option<dict> = Js.Json.decodeObject

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [Js.Dict.t(Js.Json.t)] with
a fallback.
")
let toDictOfJsonOrElse = (default, json) => json |> toDictOfJson |> Relude_Option.getOrElse(default)

@ocaml.doc("
Attempts to decode the given [Js.Json.t] value as a [Js.Dict.t(Js.Json.t)] with
a fallback of an empty [Js.Dict].
")
let toDictOfJsonOrEmpty: json => dict = toDictOfJsonOrElse(Js.Dict.empty())

////////////////////////////////////////////////////////////////////////////////
// Basic applicative-validation style decoding of arrays and objects. The
// applicative behavior of the Validation, when used in a [traverse], causes the
// errors to be collected. However, it is not possible to gather deeply-nested
// contextual error information with this style of decoding.

// These are intended to be simple and easy to use - for more advanced decoding,
// see more dedicated libraries like bs-decode.

// Note: this is using string as the error type, because allowing an arbitrary
// error makes this more complicated in terms of constructing errors, etc. It is
// possible to use an arbitrary error type, but with applicative style
// validation, the errors tend to be more informational, rather than things you
// might want to act on.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Typeclass setup for validation code
////////////////////////////////////////////////////////////////////////////////

module Error = {
  type t = string

  module Type: BsBastet.Interface.TYPE with type t = t = {
    type t = t
  }
}

module Errors = {
  // I'm standardizing on NonEmptyArray as the error collector type... this was
  // an arbitrary decision between List and Array
  type t = Relude_NonEmpty.Array.t<Error.t>

  let pure = Relude_NonEmpty.Array.pure
  let make = Relude_NonEmpty.Array.make
  let map = Relude_Validation.mapErrorsNea

  module SemigroupAny = Relude_NonEmpty.Array.SemigroupAny
}

module ValidationE = Relude_Validation.WithErrors(Errors.SemigroupAny, Error.Type)
module ArrayValidationE = Relude_Array.Validation.WithErrors(Errors.SemigroupAny, Error.Type)
module TraversableE = ArrayValidationE.Traversable

////////////////////////////////////////////////////////////////////////////////
// Basic value validation
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Validates that the given Js.Json.t value is a null
")
let validateNull: json => Relude_Validation.t<unit, Errors.t> = json =>
  toNull(json) |> Relude_Validation.fromOptionLazy(_ =>
    Errors.pure("JSON value is not a null: " ++ show(json))
  )

@ocaml.doc("
Validates that the given Js.Json.t value is a bool
")
let validateBool: json => Relude_Validation.t<bool, Errors.t> = json =>
  toBool(json) |> Relude_Validation.fromOptionLazy(_ =>
    Errors.pure("JSON value is not a bool: " ++ show(json))
  )

@ocaml.doc("
Validates that the given Js.Json.t value is a string
")
let validateString: json => Relude_Validation.t<string, Errors.t> = json =>
  toString(json) |> Relude_Validation.fromOptionLazy(_ =>
    Errors.pure("JSON value is not a string: " ++ show(json))
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an int
")
let validateInt: json => Relude_Validation.t<int, Errors.t> = json =>
  toInt(json) |> Relude_Validation.fromOptionLazy(_ =>
    Errors.pure("JSON value is not an int: " ++ show(json))
  )

@ocaml.doc("
Validates that the given Js.Json.t value is a float
")
let validateFloat: json => Relude_Validation.t<float, Errors.t> = json =>
  toFloat(json) |> Relude_Validation.fromOptionLazy(_ =>
    Errors.pure("JSON value is not a float: " ++ show(json))
  )

@ocaml.doc("
Validates that the given value is either null or can be decoded using the given
decoder.

Failed validation can be treated as None or returned as an error using the
[errorAsNone] flag.
")
let validateOptional = (
  ~errorAsNone=false,
  validate: json => Relude_Validation.t<'a, Errors.t>,
  json: json,
): Relude_Validation.t<option<'a>, Errors.t> =>
  switch validateNull(json) {
  | VOk() => VOk(None)
  | VError(_) =>
    // It was not a null, so try the real validation function
    switch validate(json) {
    | VOk(a) => Relude_Validation.pure(Some(a))
    | VError(_) as e =>
      // The value was not null, and the real validation failed - decide if we
      // ignore the error or return it
      if errorAsNone {
        Relude_Validation.pure(None)
      } else {
        e
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
// Array validation (at index and whole array)
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Validates that the given Js.Json.t value is an array, and attempts to get the
value at the given index as a raw Js.Json.t value.
")
let getJsonAtIndex: (int, json) => option<json> = (index, json) =>
  toArrayOfJson(json) |> Relude_Option.flatMap(Relude_Array.at(index))

@ocaml.doc("
Validates that the given Js.Json.t value is an array, and attempts to get the
value at the given index and validate it with the given validation function.
")
let validateJsonAtIndex: (
  int,
  json => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<'a, Errors.t> = (index, validateItem, json) =>
  getJsonAtIndex(index, json) |> Relude_Option.foldLazy(
    _ =>
      Relude_Validation.error(
        Errors.pure(string_of_int(index) ++ (" was not found in JSON: " ++ show(json))),
      ),
    json => validateItem(json),
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array with a null at the given
index.
")
let validateNullAtIndex: (int, json) => Relude_Validation.t<unit, Errors.t> = (index, json) =>
  validateJsonAtIndex(
    index,
    json =>
      validateNull(json) |> Relude_Validation.mapErrorsNea(error =>
        string_of_int(index) ++ (": " ++ error)
      ),
    json,
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array with a bool at the given
index.
")
let validateBoolAtIndex: (int, json) => Relude_Validation.t<bool, Errors.t> = (index, json) =>
  validateJsonAtIndex(
    index,
    json =>
      validateBool(json) |> Relude_Validation.mapErrorsNea(e =>
        string_of_int(index) ++ (": " ++ e)
      ),
    json,
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array with an int at the given
index.
")
let validateIntAtIndex: (int, json) => Relude_Validation.t<int, Errors.t> = (index, json) =>
  validateJsonAtIndex(
    index,
    json =>
      validateInt(json) |> Relude_Validation.mapErrorsNea(e => string_of_int(index) ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array with a float at the given
index.
")
let validateFloatAtIndex: (int, json) => Relude_Validation.t<float, Errors.t> = (index, json) =>
  validateJsonAtIndex(
    index,
    json =>
      validateFloat(json) |> Relude_Validation.mapErrorsNea(e =>
        string_of_int(index) ++ (": " ++ e)
      ),
    json,
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array with a string at the given
index.
")
let validateStringAtIndex: (int, json) => Relude_Validation.t<string, Errors.t> = (index, json) =>
  validateJsonAtIndex(
    index,
    json =>
      validateString(json) |> Relude_Validation.mapErrorsNea(e =>
        string_of_int(index) ++ (": " ++ e)
      ),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value at the given index, using the validation
function.

An invalid index can be treated as None or returned as an error using the
[missingAsNone] flag. A null value can be treated as None or returned as an
error using the [nullAsNone] flag. Failed validation can be treated as None or
returned as an error using the [errorAsNone] flag.
")
let validateOptionalAtIndex = (
  ~missingAsNone=true,
  ~nullAsNone=true,
  ~errorAsNone=false,
  index: int,
  validate: json => Relude_Validation.t<'a, Errors.t>,
  json: json,
): Relude_Validation.t<option<'a>, Errors.t> =>
  switch getJsonAtIndex(index, json) {
  | Some(json) =>
    // We got JSON at the index, see if it's null
    switch validateNull(json) {
    | VOk() =>
      // The value was null, see if we treat that as a None or an error
      if nullAsNone {
        Relude_Validation.pure(None)
      } else {
        Relude_Validation.error(
          Errors.pure(string_of_int(index) ++ (" had a null value in JSON: " ++ show(json))),
        )
      }
    | VError(_) =>
      // The value was not null, try to validate it, and see if we treat an
      // error as None or return it
      switch validate(json) {
      | VOk(a) => Relude_Validation.pure(Some(a))
      | VError(_) as e =>
        if errorAsNone {
          Relude_Validation.pure(None)
        } else {
          e
        }
      }
    }
  | None =>
    // There was no JSON at the index, see if we treat this as None or an error
    if missingAsNone {
      Relude_Validation.ok(None)
    } else {
      Relude_Validation.error(
        Errors.pure(
          "No value was found at index " ++ (string_of_int(index) ++ (" for JSON: " ++ show(json))),
        ),
      )
    }
  }

@ocaml.doc("
Validates that the given Js.Json.t value is an array, then validates each item
of the array using the given validation function.
")
let validateArrayOfJson: 'a 'e. (
  (int, json) => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<array<'a>, Errors.t> = (validateItem, json) =>
  json
  |> toArrayOfJson
  |> Relude_Option.foldLazy(
    () => Relude_Validation.error(Errors.pure("JSON value is not an array: " ++ show(json))),
    jsonValues =>
      jsonValues
      |> Relude_Array.zipWithIndex
      |> TraversableE.traverse(((json, index)) =>
        validateItem(index, json) |> Relude_Validation.mapErrorsNea(e =>
          string_of_int(index) ++ (": " ++ e)
        )
      ),
  )

@ocaml.doc("
Validates that the given Js.Json.t value is an array, then validates each item
of the array using the given validation function, then converts the result to a
list.
")
let validateArrayOfJsonAsList: 'a 'e. (
  (int, json) => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<list<'a>, Errors.t> = (validateItem, json) =>
  json
  |> toArrayOfJson
  |> Relude_Option.foldLazy(
    () => Relude_Validation.error(Errors.pure("JSON value is not an array: " ++ show(json))),
    arrayOfJson =>
      arrayOfJson
      |> Relude_Array.zipWithIndex
      |> TraversableE.traverse(((json, index)) =>
        validateItem(index, json) |> Relude_Validation.mapErrorsNea(e =>
          string_of_int(index) ++ (": " ++ e)
        )
      ),
  )
  |> Relude_Validation.map(Relude_Array.toList)

@ocaml.doc("
Validates that the given Js.Json.t value is an array, and then validates the
value at the given index is an array, and validates it using the given
validation function.
")
let validateArrayAtIndex: 'a. (
  int,
  (int, json) => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<array<'a>, Errors.t> = (index, validateItem, json) =>
  getJsonAtIndex(index, json) |> Relude_Option.foldLazy(
    _ =>
      Relude_Validation.error(
        Errors.pure(string_of_int(index) ++ (" was not found in JSON: " ++ show(json))),
      ),
    json => validateArrayOfJson(validateItem, json),
  )

@ocaml.doc("
Validates that the Js.Json.t value is an array, then validates that the value at
the given index is a Json object, and validates the object using the given
validation function.
")
let validateObjectAtIndex: 'a. (
  int,
  json => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<'a, Errors.t> = validateJsonAtIndex

////////////////////////////////////////////////////////////////////////////////
// Object validation for key and whole object
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Validates that the given Js.Json.t value is an object, and then gets the raw
Js.Json.t value for the given key.
")
let getJsonForKey: (string, json) => option<json> = (key, json) =>
  toDictOfJson(json) |> Relude_Option.flatMap(dict => Js.Dict.get(dict, key))

@ocaml.doc("
Validates that the given Js.Json.t value is an object, then validates the value
at the given key using the given validation function.
")
let validateJsonForKey: (
  string,
  json => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<'a, Errors.t> = (key, validateItem, json) =>
  getJsonForKey(key, json) |> Relude_Option.foldLazy(
    _ => Relude_Validation.error(Errors.pure(key ++ (" was not found in JSON: " ++ show(json)))),
    json => validateItem(json),
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with a null at the given key.
")
let validateNullForKey: (string, json) => Relude_Validation.t<unit, Errors.t> = (key, json) =>
  validateJsonForKey(
    key,
    json => validateNull(json) |> Relude_Validation.mapErrorsNea(e => key ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with a bool at the given key.
")
let validateBoolForKey: (string, json) => Relude_Validation.t<bool, Errors.t> = (key, json) =>
  validateJsonForKey(
    key,
    json => validateBool(json) |> Relude_Validation.mapErrorsNea(e => key ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with an int at the given key.
")
let validateIntForKey: (string, json) => Relude_Validation.t<int, Errors.t> = (key, json) =>
  validateJsonForKey(
    key,
    json => validateInt(json) |> Relude_Validation.mapErrorsNea(e => key ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with a float at the given key.
")
let validateFloatForKey: (string, json) => Relude_Validation.t<float, Errors.t> = (key, json) =>
  validateJsonForKey(
    key,
    json => validateFloat(json) |> Relude_Validation.mapErrorsNea(e => key ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with a string at the given key.
")
let validateStringForKey: (string, json) => Relude_Validation.t<string, Errors.t> = (key, json) =>
  validateJsonForKey(
    key,
    json => validateString(json) |> Relude_Validation.mapErrorsNea(e => key ++ (": " ++ e)),
    json,
  )

@ocaml.doc("
Validates the given Js.Json.t value at the given key, using the validation
function.

A missing key or null value can be treated as [None] or returned as an error
using the [missingAsNone] flag.

A null value can be treated as [None] or returned as an error using the
[nullAsNone] flag.

Failed validation can be treated as [None] or returned as an error using the
[errorAsNone] flag.
")
let validateOptionalForKey = (
  ~missingAsNone=true,
  ~nullAsNone=true,
  ~errorAsNone=false,
  key: string,
  validate: json => Relude_Validation.t<'a, Errors.t>,
  json: json,
): Relude_Validation.t<option<'a>, Errors.t> =>
  // Try to get the JSON for the key
  switch getJsonForKey(key, json) {
  | Some(json) =>
    // We got JSON for the key - see if it's null
    switch validateNull(json) {
    | VOk() =>
      // The value was null - see if we treat this as a None or an error
      if nullAsNone {
        Relude_Validation.pure(None)
      } else {
        Relude_Validation.error(
          Errors.pure(key ++ (" contained a null value in JSON: " ++ show(json))),
        )
      }
    | VError(_) =>
      // The value was not null - try to decode and see if we treat an error as
      // None or as an error
      switch validate(json) {
      | VOk(a) => Relude_Validation.pure(Some(a))
      | VError(_) as e =>
        if errorAsNone {
          Relude_Validation.pure(None)
        } else {
          e
        }
      }
    }
  | None =>
    // No JSON found for key - see if we treat that as a None or an error
    if missingAsNone {
      Relude_Validation.ok(None)
    } else {
      Relude_Validation.error(Errors.pure(key ++ (" was not found in JSON: " ++ show(json))))
    }
  }

@ocaml.doc("
Validates the given Js.Json.t value is an object with an array at the given key,
then validates the array using the given validation function.
")
let validateArrayForKey: 'a. (
  string,
  (int, json) => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<array<'a>, Errors.t> = (key, validateItem, json) =>
  getJsonForKey(key, json) |> Relude_Option.foldLazy(
    _ => Relude_Validation.error(Errors.pure(key ++ (" was not found in JSON: " ++ show(json)))),
    json => validateArrayOfJson(validateItem, json),
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with an array at the given key,
then validates the array using the given validation function, returning the
result in a list.
")
let validateListForKey: 'a. (
  string,
  (int, json) => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<list<'a>, Errors.t> = (key, validateItem, json) =>
  getJsonForKey(key, json) |> Relude_Option.foldLazy(
    _ => Relude_Validation.error(Errors.pure(key ++ (" was not found in JSON: " ++ show(json)))),
    json => validateArrayOfJsonAsList(validateItem, json),
  )

@ocaml.doc("
Validates the given Js.Json.t value is an object with an object at the given
key, then validates the object using the given validation function.
")
let validateObjectForKey: 'a. (
  string,
  json => Relude_Validation.t<'a, Errors.t>,
  json,
) => Relude_Validation.t<'a, Errors.t> = validateJsonForKey

////////////////////////////////////////////////////////////////////////////////
// JSON DSL
////////////////////////////////////////////////////////////////////////////////

@ocaml.doc("
Exposes a set of focused/abbreviated operations for use as a global or local
open.

The map/apply operators are provided for applicative style validation
for decoding objects.
")
module DSL = {
  // Note: decoding an object can be done using the map-ap-ap-ap-... approach
  // with the <$> (map) and <*> (apply) operators for validation.

  // Bring some infix operators into scope
  let \"<$>" = ValidationE.Infix.\"<$>" // map
  let \"<$$>" = ValidationE.Infix.\"<$$>" // flipMap - useful for mapping values after decoding them
  let \"<*>" = ValidationE.Infix.\"<*>" // apply
  let \">>=" = ValidationE.Infix.\">>=" // bind

  @ocaml.doc("
  JSON encode utilities
  ")
  module JE = {
    @ocaml.doc("
    Creates a JSON null value
    ")
    let null: json = null

    @ocaml.doc("
    Encodes a bool as a JSON bool value
    ")
    let bool: bool => json = fromBool

    @ocaml.doc("
    Encodes an int as a JSON number value
    ")
    let int: int => json = fromInt

    @ocaml.doc("
    Encodes a float as a JSON number value
    ")
    let float: float => json = fromFloat

    @ocaml.doc("
    Encodes a float as a JSON number value

    Alias of [float]
    ")
    let num: float => json = fromFloat

    @ocaml.doc("
    Encodes a string as a JSON string value
    ")
    let string: string => json = fromString

    @ocaml.doc("
    Encodes an option('a) as JSON, using the given encode function, or returning
    null for None.
    ")
    let opt: ('a => json, option<'a>) => json = fromOption

    @ocaml.doc("
    Encodes an array(Js.Json.t) as a single Js.Json.t (array) value
    ")
    let array: array<json> => json = fromArrayOfJson

    @ocaml.doc("
    Maps a JSON-conversaion function over an array of values, and then encodes
    the result as a JSON array
    ")
    let arrayBy: 'a. ('a => json, array<'a>) => json = fromArrayOfJsonBy

    @ocaml.doc("
    Encodes an array(Js.Dict.t(Js.Json.t)) into a Js.Json.t value
    ")
    let arrayOfDict: array<dict> => json = fromArrayOfDictOfJson

    @ocaml.doc("
    Encodes an array of key/value pairs into a Js.Json.t value
    ")
    let arrayOfTuples: array<(Js.Dict.key, json)> => json = fromArrayOfKeyValueTuples

    @ocaml.doc("
    Encodes a list of Js.Json.t values to a Js.Json.t value
    ")
    let list: list<json> => json = fromListOfJson

    @ocaml.doc("
    Maps a JSON-conversion function over a list of values, then encodes the
    result as a Js.Json.t array value
    ")
    let listBy = fromListOfJsonBy

    @ocaml.doc("
    Encodes a list Js.Dict.t(Js.Json.t) values into a Js.Json.t array value
    ")
    let listOfDict: list<dict> => json = fromListOfDictOfJson

    @ocaml.doc("
    Encodes a list of key/value pairs into a Js.Json.t array value
    ")
    let listOfTuples: list<(Js.Dict.key, json)> => json = fromListOfKeyValueTuples

    @ocaml.doc("
    Encodes a dict of Js.Json.t values into a Js.Json.t value
    ")
    let dict: dict => json = fromDictOfJson
  }

  @ocaml.doc("
  JSON decoding utilities
  ")
  module JD = {
    ////////////////////////////////////////////////////////////////////////////
    // Plain value validation (decoding)
    ////////////////////////////////////////////////////////////////////////////

    @ocaml.doc("
    Validates the given Js.Json.t value is a null
    ")
    let null: json => Relude_Validation.t<unit, Errors.t> = validateNull

    @ocaml.doc("
    Validates the given Js.Json.t value is a bool
    ")
    let bool: json => Relude_Validation.t<bool, Errors.t> = validateBool

    @ocaml.doc("
    Validates the given Js.Json.t value as a string
    ")
    let int: json => Relude_Validation.t<int, Errors.t> = validateInt

    @ocaml.doc("
    Validates the given Js.Json.t value as a float
    ")
    let float: json => Relude_Validation.t<float, Errors.t> = validateFloat

    @ocaml.doc("
    Validates the given Js.Json.t value as a string
    ")
    let string: json => Relude_Validation.t<string, Errors.t> = validateString

    @ocaml.doc("
    Validates that the given Js.Json.t value is either null or can be validated
    using the given function.

    If the validation function fails, the error can either be returned as a
    successful None value, or the error can be returned - depending on the
    [errorAsNone] flag.
    ")
    let opt = (
      ~errorAsNone=false,
      validate: json => Relude_Validation.t<'a, Errors.t>,
      json: json,
    ): Relude_Validation.t<option<'a>, Errors.t> => validateOptional(~errorAsNone, validate, json)

    ////////////////////////////////////////////////////////////////////////////////
    // Array validation - validating items by array index (or whole array)
    ////////////////////////////////////////////////////////////////////////////////

    @ocaml.doc("
    Gets the Js.Json.t value at the given index of a Js.Json.t array
    ")
    let getAt: (int, json) => option<json> = getJsonAtIndex

    @ocaml.doc("
    Validates the Js.Json.t value at the given index with the given validation
    function
    ")
    let jsonAt: 'a. (
      int,
      json => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<'a, Errors.t> = validateJsonAtIndex

    @ocaml.doc("
    Validates a null value at the given index of a Js.Json.t array value
    ")
    let nullAt: (int, json) => Relude_Validation.t<unit, Errors.t> = validateNullAtIndex

    @ocaml.doc("
    Validates a bool value at the given index of a Js.Json.t array value
    ")
    let boolAt: (int, json) => Relude_Validation.t<bool, Errors.t> = validateBoolAtIndex

    @ocaml.doc("
    Validates a string value at the given index of a Js.Json.t array value
    ")
    let stringAt: (int, json) => Relude_Validation.t<string, Errors.t> = validateStringAtIndex

    @ocaml.doc("
    Validates an int value at the given index of a Js.Json.t array value
    ")
    let intAt: (int, json) => Relude_Validation.t<int, Errors.t> = validateIntAtIndex

    @ocaml.doc("
    Validates a float value at the given index of a Js.Json.t array value
    ")
    let floatAt: (int, json) => Relude_Validation.t<float, Errors.t> = validateFloatAtIndex

    @ocaml.doc("
    Validates that the Js.Json.t value at the given index is either null or can
    be validated using the given function.

    - Bad indices can be treated as None or as an error using [missingAsNone]
    - Null value can be treated as None or as an error using [nullAsNone]
    - Failed validation can be treated as None or an error using [perrorAsNone]
    ")
    let optAt = (
      ~missingAsNone=true,
      ~nullAsNone=true,
      ~errorAsNone=false,
      index: int,
      validate: json => Relude_Validation.t<'a, Errors.t>,
      json: json,
    ): Relude_Validation.t<option<'a>, Errors.t> =>
      validateOptionalAtIndex(~missingAsNone, ~nullAsNone, ~errorAsNone, index, validate, json)

    @ocaml.doc("
    Validates an array at the given index of a Js.Json.t array value
    ")
    let arrayAt: 'a. (
      int,
      (int, json) => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<array<'a>, Errors.t> = validateArrayAtIndex

    @ocaml.doc("
    Validates an obejct at the given index of a Js.Json.t array value
    ")
    let objectAt: 'a. (
      int,
      json => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<'a, Errors.t> = validateObjectAtIndex

    @ocaml.doc("
    Validates an Js.Json.t array using the given validation function
    ")
    let array: 'a. (
      (int, json) => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<array<'a>, Errors.t> = validateArrayOfJson

    @ocaml.doc("
    Validates an Js.Json.t object using the given validation function
    ")
    let list: 'a. (
      (int, json) => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<list<'a>, Errors.t> = validateArrayOfJsonAsList

    ////////////////////////////////////////////////////////////////////////////////
    // Object validation - validating items by object key
    ////////////////////////////////////////////////////////////////////////////////

    @ocaml.doc("
    Gets the Js.Json.t value for the given key of a Js.Json.t object value
    ")
    let getFor: (string, json) => option<json> = getJsonForKey

    @ocaml.doc("
    Validates the Js.Json.t value for the given key, using the given validation
    function
    ")
    let jsonFor: 'a. (
      string,
      json => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<'a, Errors.t> = validateJsonForKey

    @ocaml.doc("
    Validates a null for the given key of a Js.Json.t object value
    ")
    let nullFor: (string, json) => Relude_Validation.t<unit, Errors.t> = validateNullForKey

    @ocaml.doc("
    Validates a bool for the given key of a Js.Json.t object value
    ")
    let boolFor: (string, json) => Relude_Validation.t<bool, Errors.t> = validateBoolForKey

    @ocaml.doc("
    Validates a string for the given key of a Js.Json.t object value
    ")
    let stringFor: (string, json) => Relude_Validation.t<string, Errors.t> = validateStringForKey

    @ocaml.doc("
    Validates an int for the given key of a Js.Json.t object value
    ")
    let intFor: (string, json) => Relude_Validation.t<int, Errors.t> = validateIntForKey

    @ocaml.doc("
    Validates a float for the given key of a Js.Json.t object value
    ")
    let floatFor: (string, json) => Relude_Validation.t<float, Errors.t> = validateFloatForKey

    @ocaml.doc("
    Validates that the Js.Json.t value at the given key is either null or can be
    validated using the given function.

    - Bad indices can be treated as None or as an error using [missingAsNone]
    - Null value can be treated as None or as an error using [nullAsNone]
    - Failed validation can be treated as None or an error using [errorAsNone]
    ")
    let optFor = (
      ~missingAsNone=true,
      ~nullAsNone=true,
      ~errorAsNone=false,
      key: string,
      validate: json => Relude_Validation.t<'a, Errors.t>,
      json: json,
    ): Relude_Validation.t<option<'a>, Errors.t> =>
      validateOptionalForKey(~missingAsNone, ~nullAsNone, ~errorAsNone, key, validate, json)

    @ocaml.doc("
    Validates an array for the given key of a Js.Json.t object value
    ")
    let arrayFor: 'a. (
      string,
      (int, json) => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<array<'a>, Errors.t> = validateArrayForKey

    @ocaml.doc("
    Validates an array for the given key of a Js.Json.t object value, as a list
    ")
    let listFor: 'a. (
      string,
      (int, json) => Relude_Validation.t<'a, Errors.t>,
      json,
    ) => Relude_Validation.t<list<'a>, Errors.t> = validateListForKey

    @ocaml.doc("
    Validates an object for the given key of a Js.Json.t object value
    ")
    let objectFor = validateObjectForKey
  }
}
