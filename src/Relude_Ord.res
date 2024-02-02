@@uncurried
@@uncurried.swap

open BsBastet.Interface

@@ocaml.text(`
[Relude.Ord] contains functions and sub-modules to help work with comparison
functions that take two values of the same type and return a value of [Bastet]'s
[ordering] type.
`)

@ocaml.doc("
The ordering type represents the result of a comparison - [`less_than],
[`equal_to], or [`greater_than].
")
type ordering = ordering

@ocaml.doc("
The compare function is the heart of the [Ord] type class.
")
type compare<'a> = ('a, 'a) => ordering

@ocaml.doc("
[Ord.by] converts an compare function for a type ['a] to a compare function of
type ['b] using a function from ['b => 'a].

If we know how to compare ['a]s and we know how to convert ['b] to ['a], we can
compare ['b]s by first converting our ['b]s to ['a]s, then comparing them with
the ['a] compare function.

This is the contravariant map for the compare function.

{[
  let userCompare: compare(user) = Ord.by(user => user.name, String.compare);
]}
")
let by: 'a 'b. ('b => 'a, compare<'a>) => compare<'b> = (bToA, compareA) => (b1, b2) =>
  compareA(bToA(b1), bToA(b2))

@ocaml.doc("
[Ord.cmap] is an alias for {!val:by} and the foundational function of the
[Contravariant] type class instance for the {!type:compare} function.
")
let cmap = by

module Contravariant: CONTRAVARIANT with type t<'a> = compare<'a> = {
  type t<'a> = compare<'a>
  let cmap = by
}
include Relude_Extensions_Contravariant.ContravariantExtensions(Contravariant)

@ocaml.doc("
[Ord.reverse] creates a new compare function that returns the opposite of the
given compare function.
")
let reverse: 'a. compare<'a> => compare<'a> = compare => (a1, a2) =>
  switch compare(a1, a2) {
  | #less_than => #greater_than
  | #equal_to => #equal_to
  | #greater_than => #less_than
  }

@ocaml.doc("
[Ord.compareAsIntBy] compares two values using the given compare function,
returning [1] if the first value is greater than the second, [-1] if the first
is less than the second, and [0] if they are equal.

While not often needed in the Relude/Bastet ecosystem, this function may be
useful for translating a Bastet-compatible [compare] function to one that works
with the rest of the OCaml world.
")
let compareAsIntBy: 'a. (compare<'a>, 'a, 'a) => int = (compare, a, b) =>
  switch compare(a, b) {
  | #less_than => -1
  | #equal_to => 0
  | #greater_than => 1
  }

@ocaml.doc("
Compares two values using the given ORD module
")
let compareAsInt = (type a, ord: module(ORD with type t = a), a: a, b: a): int => {
  module Ord = unpack(ord)
  compareAsIntBy(Ord.compare, a, b)
}

@ocaml.doc("
Finds the minimum of two values using the given compare function
")
let minBy: 'a. (compare<'a>, 'a, 'a) => 'a = (compare, a, b) =>
  switch compare(a, b) {
  | #greater_than => b
  | #less_than
  | #equal_to => a
  }

@ocaml.doc("
Finds the minimum of two values using the given ORD module
")
let min = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  minBy(Ord.compare, a, b)
}

@ocaml.doc("
Finds the maximum of two values using the given compare function
")
let maxBy: 'a. (compare<'a>, 'a, 'a) => 'a = (compare, a, b) =>
  switch compare(a, b) {
  | #less_than => b
  | #greater_than
  | #equal_to => a
  }

@ocaml.doc("
Finds the maximum of two values using the given ORD module
")
let max = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  maxBy(Ord.compare, a, b)
}

@ocaml.doc("
Indicates if the item on the left is less than the item on the right using the
given compare function.
")
let lessThanBy: 'a. (compare<'a>, 'a, 'a) => bool = (compare, a, b) => compare(a, b) == #less_than

@ocaml.doc("
[Ord.ltBy] is an alias for {!val:lessThanBy}.
")
let ltBy = lessThanBy

@ocaml.doc("
[Ord.lessThan] indicates if the item on the left is less than the item on the
right using the given ORD module.
")
let lessThan = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  lessThanBy(Ord.compare, a, b)
}

@ocaml.doc("
[Ord.lt] is an alias for {!val:lessThan}.
")
let lt = lessThan

@ocaml.doc("
[Ord.lessThanOrEqBy] indicates if the item on the left is less than or equal to
the item on the right using the given compare function.
")
let lessThanOrEqBy: 'a. (compare<'a>, 'a, 'a) => bool = (compare, a, b) =>
  compare(a, b) != #greater_than

@ocaml.doc("
[Ord.lteBy] is an alias for {!val:lessThanOrEqBy}.
")
let lteBy = lessThanOrEqBy

@ocaml.doc("
[Ord.lessThanOrEq] indicates if the item on the left is less than or equal to
the item on the right using the given ORD module.
")
let lessThanOrEq = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  lessThanOrEqBy(Ord.compare, a, b)
}

@ocaml.doc("
[Ord.lte] is an alias for {!val:lessThanOrEq}.
")
let lte = lessThanOrEq

@ocaml.doc("
[Ord.greaterThanBy] indicates if the item on the left is greater than the item
on the right using the given compare function.
")
let greaterThanBy: 'a. (compare<'a>, 'a, 'a) => bool = (compare, a, b) =>
  compare(a, b) == #greater_than

@ocaml.doc("
[Ord.gtBy] is an alias for {!val:greaterThanBy}.
")
let gtBy = greaterThanBy

@ocaml.doc("
[Ord.greaterThan] indicates if the item on the left is greater than the item
on the right using the given ORD module.
")
let greaterThan = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  greaterThanBy(Ord.compare, a, b)
}

@ocaml.doc("
[Ord.gt] is an alias for {!val:greaterThan}.
")
let gt = greaterThan

@ocaml.doc("
[Ord.greaterThanOrEqBy] indicates if the item on the left is greater than or
equal to the item on the right using the given compare function.
")
let greaterThanOrEqBy: 'a. (compare<'a>, 'a, 'a) => bool = (compare, a, b) =>
  compare(a, b) != #less_than

@ocaml.doc("
[Ord.gteBy] is an alias for {!val:greaterThanOrEqBy}.
")
let gteBy = greaterThanOrEqBy

@ocaml.doc("
[Ord.greaterThanOrEq] indicates if the item on the left is greater than or equal
to the item on the right using the given ORD module.
")
let greaterThanOrEq = (type a, ord: module(ORD with type t = a), a, b) => {
  module Ord = unpack(ord)
  greaterThanOrEqBy(Ord.compare, a, b)
}

@ocaml.doc("
[Ord.gte] is an alias for {!val:greaterThanOrEq}.
")
let gte = greaterThanOrEq

@ocaml.doc("
[Ord.clampBy] ensures a provided value falls between a max and min (inclusive).

Note that if the provided min is greater than the provided max, the max is
always returned. This is considered an incorrect use of [clamp].

{[
  let clamp = clampBy(Int.compare);
  clamp(~min=0, ~max=5, 3) == 3;
  clamp(~min=0, ~max=5, 0) == 0;
  clamp(~min=0, ~max=3, 4) == 3;
  clamp(~min=1, ~max=0, 2) == 0; // don't do this
]}
")
let clampBy: 'a. (compare<'a>, ~min: 'a, ~max: 'a, 'a) => 'a = (compare, ~min, ~max, v) =>
  minBy(compare, max, maxBy(compare, min, v))

@ocaml.doc("
[Ord.clamp] is the first-class module version of {!val:clampBy}.
")
let clamp = (type a, ord: module(ORD with type t = a), ~min: a, ~max: a, x) => {
  module Ord = unpack(ord)
  clampBy(Ord.compare, ~min, ~max, x)
}

@ocaml.doc("
[Ord.betweenBy] determines whether a provided value falls between a min and max.
")
let betweenBy: 'a. (compare<'a>, ~min: 'a, ~max: 'a, 'a) => bool = (compare, ~min, ~max, v) =>
  greaterThanOrEqBy(compare, v, min) && lessThanOrEqBy(compare, v, max)

@ocaml.doc("
[Ord.between] is the first-class module version of {!val:betweenBy}.
")
let between = (type a, ord: module(ORD with type t = a), ~min: a, ~max: a, x) => {
  module Ord = unpack(ord)
  betweenBy(Ord.compare, ~min, ~max, x)
}

@ocaml.doc("
[Ord.abs] determines the absolute value for any value that can be compared for
ordering and implements Ring (specifically has [zero] and [subtract]). If the
provided value is greater than (or equal to) zero, that value is returned,
otherwise the negated version of that value is returned.
")
let abs = (type a, ord: module(ORD with type t = a), ring: module(RING with type t = a), x) => {
  module Ring = unpack(ring)
  gte(ord, x, Ring.zero) ? x : Ring.subtract(Ring.zero, x)
}

@ocaml.doc("
[Ord.signum] is the sign function, which evaluates to one for values >= zero,
and negative one for values less than zero.
")
let signum = (
  type a,
  ord: module(ORD with type t = a),
  ring: module(RING with type t = a),
  x,
): a => {
  module Ring = unpack(ring)
  open Ring
  gte(ord, x, zero) ? one : subtract(zero, one)
}
