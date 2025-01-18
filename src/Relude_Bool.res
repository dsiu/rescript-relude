open BsBastet.Interface

@@ocaml.text(`
[Relude.Bool] contains typeclass instances and utility functions for working
with the [bool] type.
`)

@ocaml.doc("
Folds a bool value into a value of a different type, using a function for the
true and false cases.

{[
  Bool.ifElse(() => \"yes\", () => \"no\", true) == \"yes\";
  Bool.ifElse(() => \"yes\", () => \"no\", false) == \"no\";
]}
")
let ifElse: (unit => 'a, unit => 'a, bool) => 'a = (onTrue, onFalse, value) =>
  if value {
    onTrue()
  } else {
    onFalse()
  }

@ocaml.doc("
[Bool.inverse] negates the boolean.

This function isn't named [not], because refmt would rewrite it as [(!)].
")
let inverse: bool => bool = not

@ocaml.doc("
[Bool.not_] is an alias for {!val:inverse}.
")
let not__: bool => bool = inverse

@ocaml.doc("
Combines two boolean using an AND
")
let and_: (bool, bool) => bool = \"&&"

@ocaml.doc("
Combines two boolean using an OR
")
let or_: (bool, bool) => bool = \"||"

@ocaml.doc("
Combines two booleans using an NAND
")
let nand: (bool, bool) => bool = (a, b) => !(a && b)

@ocaml.doc("
Combines two booleans using an NOR
")
let nor: (bool, bool) => bool = (a, b) => !(a || b)

@ocaml.doc("
Combines two booleans using an XOR
")
let xor: (bool, bool) => bool = (a, b) => (!a && b) || (a && !b)

@ocaml.doc("
Combines two booleans using an XNOR
")
let xnor: (bool, bool) => bool = (a, b) => !xor(a, b)

@ocaml.doc("
Combines two booleans using an implication
")
let implies: (bool, bool) => bool = (a, b) => !a || b

@ocaml.doc("
Compares two booleans for equality
")
let eq: (bool, bool) => bool = (a, b) =>
  switch (a, b) {
  | (true, true) => true
  | (false, false) => true
  | (true, false) => false
  | (false, true) => false
  }

@ocaml.doc("
EQ instance for booleans
")
module Eq: EQ with type t = bool = {
  type t = bool
  let eq = eq
}

@ocaml.doc("
Compares two booleans for equality
")
let compare: (bool, bool) => ordering = (a, b) =>
  switch (a, b) {
  | (true, true) => #equal_to
  | (false, false) => #equal_to
  | (true, false) => #greater_than
  | (false, true) => #less_than
  }

@ocaml.doc("
ORD instance for booleans
")
module Ord: ORD with type t = bool = {
  include Eq
  let compare = compare
}

@ocaml.doc("
Converts a boolean value to a string
")
let show: bool => string = b => b ? "true" : "false"

@ocaml.doc("
SHOW instance for booleans
")
module Show: SHOW with type t = bool = {
  type t = bool
  let show = show
}

module Conjunctive = {
  module Magma: MAGMA with type t = bool = {
    type t = bool
    let append = \"&&"
  }

  module MedialMagma: MEDIAL_MAGMA with type t = bool = Magma

  module Semigroup: SEMIGROUP with type t = bool = Magma
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup)

  module Monoid: MONOID with type t = bool = {
    include Semigroup
    let empty = true
  }
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid)
}

module And = Conjunctive

module Disjunctive = {
  module Magma: MAGMA with type t = bool = {
    type t = bool
    let append = \"||"
  }

  module MedialMagma: MEDIAL_MAGMA with type t = bool = Magma

  module Semigroup: SEMIGROUP with type t = bool = Magma
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup)

  module Monoid: MONOID with type t = bool = {
    include Semigroup
    let empty = false
  }
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid)
}

module Or = Disjunctive

module Bounded: BOUNDED with type t = bool = {
  include Ord
  let top = true
  let bottom = false
}
include Relude_Extensions_Bounded.BoundedExtensions(Bounded)

module Enum: Relude_Interface.ENUM with type t = bool = {
  include Ord
  let pred = x =>
    switch x {
    | true => Some(false)
    | false => None
    }
  let succ = x =>
    switch x {
    | true => None
    | false => Some(true)
    }
}
include Relude_Extensions_Enum.EnumExtensions(Enum)

module BoundedEnum: Relude_Interface.BOUNDED_ENUM with type t = bool = {
  include Bounded
  include (Enum: Relude_Interface.ENUM with type t := t)
  let cardinality = 2
  let fromEnum = x =>
    switch x {
    | false => 0
    | true => 1
    }
  let toEnum = x =>
    switch x {
    | 0 => Some(false)
    | 1 => Some(true)
    | _ => None
    }
}
include Relude_Extensions_BoundedEnum.BoundedEnumExtensions(BoundedEnum)

module Infix = {
  include Relude_Extensions_Eq.EqInfix(Eq)
  include Relude_Extensions_Ord.OrdInfix(Ord)
}
