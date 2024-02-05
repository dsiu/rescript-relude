@@uncurried
@@uncurried.swap

@@ocaml.text(`
[Relude.Decimal] contains a type [t] which represents arbitrary precision
numeric values, backed by an [int] mantissa and an [int] exponent. This can be
useful for representing currency values without loss in precision or floating
point errors.

Because the mantissa and exponent are backed by ints, there is a fairly
restrictive range of values that can be represented.

{[
  let a = Decimal(12345, -2); // 123.45
  let b = Decimal(6789, 3); // 6789000
]}
`)

@ocaml.doc("
The type of the base of our Decimal
")
type mantissa = int

@ocaml.doc("
The type of the exponent of our Decimal
")
type exponent = int

@ocaml.doc("
Represents an arbitrary precision number, backed by an integer mantissa, and
integer exponent.
")
type t = Decimal(mantissa, exponent)

@ocaml.doc("
Represents a preference for rounding a [Decimal] value, when applicable.
")
type rounding =
  | Truncate
  | RoundUp
  | RoundDown
  | RoundUpOrDown

@ocaml.doc("
[Decimal.make] constructs a [Decimal] from a [mantissa] and [exponent].
")
let make: (int, int) => t = (mantissa, exponent) => Decimal(mantissa, exponent)

@ocaml.doc("
[Decimal.fromInt] constructs a [Decimal] from an [int].
")
let fromInt: int => t = intValue => Decimal(intValue, 0)

@ocaml.doc("
[Decimal.fromString] attempts to parse a [Decimal] from a [string]
")
@ocaml.doc("
[Decimal.show] renders the [Decimal] value to a [string], as if the [Decimal]
was represented as a [float].

{[
  Decimal(12345, -2) |> Decimal.toString // \"123.45\"
  Decimal(12345, 3) |> Decimal.toString // \"12345000\"
]}
")
let // let fromString: string => option(t) = _ => None; // TODO

show: t => string = (Decimal(mantissa, exponent)) =>
  if exponent == 0 {
    string_of_int(mantissa)
  } else if exponent > 0 {
    if mantissa == 0 {
      "0"
    } else {
      let zeroes = Relude_String.repeat(exponent, "0")
      string_of_int(mantissa) ++ zeroes
    }
  } else {
    /* mantissa < 0 */
    let (whole, fractional) = Relude_String.splitAt(exponent, string_of_int(mantissa))
    whole ++ ("." ++ fractional)
  }

@ocaml.doc("
[Decimal.round] rounds a [Decimal] using the given rounding strategy.
")
@ocaml.doc("
[Decimal.tenToThePowerOfPositive] computes the value of [10^exponent].

The return value of this function is undefined for exponent values < 0.

{[
  Decimal.tenToThePowerOf(3) == 1000;
]}
")
let // let round: (rounding, t) => t = (_rounding, decimal) => decimal; // TODO

tenToThePowerOfPositive: int => int = exponent =>
  Relude_Int.rangeAsArray(1, exponent + 1)->(Relude_Array.foldLeft((acc, _) => 10 * acc, 1, _))

@ocaml.doc("
[Decimal.normalize] normalizes the exponent to the minimal exponent for two
given [Decimal] values.

{[
  let a = Decimal(12345, -2); // 123.45
  let b = Decimal(12345, 3); // 12345000
  let res = Decimal.normalize(a, b);

  res == (Decimal(12345, -2), Decimal(1234500000, -2), -2)
]}
")
let normalize: (t, t) => (t, t, int) = (
  Decimal(mantissaA, exponentA),
  Decimal(mantissaB, exponentB),
) => {
  let exponentMin = Relude_Int.min(exponentA, exponentB)
  let newMantissaA = mantissaA * tenToThePowerOfPositive(exponentA - exponentMin)
  let newMantissaB = mantissaB * tenToThePowerOfPositive(exponentB - exponentMin)
  (Decimal(newMantissaA, exponentMin), Decimal(newMantissaB, exponentMin), exponentMin)
}

@ocaml.doc("
[Decimal.add] adds two [Decimal] values with no attempt at avoiding overflow.

Note: the arguments are in order of [lhs], [rhs].
")
let add: (t, t) => t = (lhs, rhs) => {
  let (Decimal(mantissaLHS, _), Decimal(mantissaRHS, _), exponent) = normalize(lhs, rhs)
  Decimal(mantissaLHS + mantissaRHS, exponent)
}

@ocaml.doc("
[Decimal.(+..)] is the infix operator for [add].
")
let \"+.." = add

@ocaml.doc("
[Decimal.subtract] subtracts two [Decimal] values with no attempt at avoiding
overflow.

Note: the arguments are in order of [lhs], [rhs].
")
let subtract: (t, t) => t = (lhs, rhs) => {
  let (Decimal(mantissaLHS, _), Decimal(mantissaRHS, _), exponent) = normalize(lhs, rhs)
  Decimal(mantissaLHS - mantissaRHS, exponent)
}

@ocaml.doc("
[Decimal.(-..)] is the infix operator for [subtract].
")
let \"-.." = subtract

@@ocaml.doc("
[Decimal.multiply] multiplies two [Decimal] values with no attempt at avoiding
overflow.

Note: the arguments are in order of [lhs], [rhs]
")
@@ocaml.doc(// TODO
//  let multiply: (t, t) => t =
//    (lhs, _rhs) => {
//      lhs;
//    };

"
Infix operator for [multiply]
")
@@ocaml.doc(//let ( *.. ) = multiply;

"
Divides two [Decimal] values using the given [rounding] preference.

Note: the arguments are in order of [lhs], [rhs]
")
@@ocaml.doc(// TODO
//  let divide: (t, t, rounding) => t =
//    (lhs, _rhs, _rounding) => {
//      lhs;
//    };

"
Infix operator for [divide]
" /* let (/..) = divide */)
