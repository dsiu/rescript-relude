@@ocaml.text(`
Contains functions and typeclass instances for the [ordering] type - the enum
with inhabitants [\`less_than], [\`equal_to], and [\`greater_than].

Functions that deal with comparing values don't belong here - these go in
{!module:Relude_Ord} and {!module:Relude_Extensions_Ord}. This module is just
for working with values of type [ordering].
`)

type t = BsBastet.Interface.ordering

@ocaml.doc("
[Ordering.fromInt] converts an int to a type-safe ordering value. This can be
helpful for working with other functions in the OCaml world that normally use
[-1] to indicate [`less_than], [0] to indicate [`equal_to], and [1] to indicate
[`greater_than].
")
let fromInt: int => t = x =>
  switch x {
  | i if i < 0 => #less_than
  | i if i == 0 => #equal_to
  | _ => #greater_than
  }

@ocaml.doc("
[Ordering.toInt] converts a type-safe ordering value to an int. This is useful
when working with OCaml libraries that expect [-1] for {e less than}, [0] for
{e equal} and [1] for {e greater than}.
")
let toInt: t => int = x =>
  switch x {
  | #less_than => -1
  | #equal_to => 0
  | #greater_than => 1
  }

@ocaml.doc("
[Ordering.reverse] reverses the value of the ordering ([`greater_than] becomes
[`less_than] and vice versa).
")
let reverse: t => t = x =>
  switch x {
  | #less_than => #greater_than
  | #equal_to => #equal_to
  | #greater_than => #less_than
  }

@ocaml.doc("
[Ordering.eq] compares two orderings for equality
")
let eq: (t, t) => bool = (a, b) => a == b

module Eq: BsBastet.Interface.EQ with type t = t = {
  type t = t
  let eq = eq
}
include Relude_Extensions_Eq.EqExtensions(Eq)

@ocaml.doc("
[Ordering.compare] ompares two orderings
([`less_than < `equal_to < `greater_than]).
")
let compare: (t, t) => t = (o1, o2) =>
  switch (o1, o2) {
  | (#less_than, #less_than) => #equal_to
  | (#less_than, #equal_to) => #less_than
  | (#less_than, #greater_than) => #less_than
  | (#equal_to, #less_than) => #greater_than
  | (#equal_to, #equal_to) => #equal_to
  | (#equal_to, #greater_than) => #less_than
  | (#greater_than, #less_than) => #greater_than
  | (#greater_than, #equal_to) => #greater_than
  | (#greater_than, #greater_than) => #equal_to
  }

module Ord: BsBastet.Interface.ORD with type t = t = {
  include Eq
  let compare = compare
}
include Relude_Extensions_Ord.OrdExtensions(Ord)

let top = #greater_than

let bottom = #less_than

module Bounded: BsBastet.Interface.BOUNDED with type t = t = {
  include Ord
  let top = top
  let bottom = bottom
}
include Relude_Extensions_Bounded.BoundedExtensions(Bounded)

let pred: t => option<t> = x =>
  switch x {
  | #less_than => None
  | #equal_to => Some(#less_than)
  | #greater_than => Some(#equal_to)
  }

let succ: t => option<t> = x =>
  switch x {
  | #less_than => Some(#equal_to)
  | #equal_to => Some(#greater_than)
  | #greater_than => None
  }

module Enum: Relude_Interface.ENUM with type t = t = {
  include Ord
  let pred = pred
  let succ = succ
}
include Relude_Extensions_Enum.EnumExtensions(Enum)

module BoundedEnum: Relude_Interface.BOUNDED_ENUM with type t = t = {
  include Bounded
  include (Enum: Relude_Interface.ENUM with type t := t)
  let cardinality = 3
  let toEnum = i => Some(fromInt(i))
  let fromEnum = toInt
}
include Relude_Extensions_BoundedEnum.BoundedEnumExtensions(BoundedEnum)
