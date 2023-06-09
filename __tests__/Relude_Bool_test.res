open Jest
open Expect

module Bool = Relude.Bool

describe("Bool", () => {
  testAll("ifElse", list{(true, "yes"), (false, "no")}, ((input, expected)) =>
    expect(input |> Bool.ifElse(() => "yes", () => "no")) |> toEqual(expected)
  )

  testAll("inverse", list{(true, false), (false, true)}, ((input, expected)) =>
    expect(Bool.inverse(input)) |> toEqual(expected)
  )

  testAll(
    "and_",
    list{(true, true, true), (true, false, false), (false, true, false), (false, false, false)},
    ((a, b, c)) => expect(Bool.and_(a, b)) |> toEqual(c),
  )

  testAll(
    "or_",
    list{(true, true, true), (true, false, true), (false, true, true), (false, false, false)},
    ((a, b, c)) => expect(Bool.or_(a, b)) |> toEqual(c),
  )

  testAll(
    "nand",
    list{(true, true, false), (true, false, true), (false, true, true), (false, false, true)},
    ((a, b, c)) => expect(Bool.nand(a, b)) |> toEqual(c),
  )

  testAll(
    "nor_",
    list{(true, true, false), (true, false, false), (false, true, false), (false, false, true)},
    ((a, b, c)) => expect(Bool.nor(a, b)) |> toEqual(c),
  )

  testAll(
    "xor",
    list{(true, true, false), (true, false, true), (false, true, true), (false, false, false)},
    ((a, b, c)) => expect(Bool.xor(a, b)) |> toEqual(c),
  )

  testAll(
    "xnor",
    list{(true, true, true), (true, false, false), (false, true, false), (false, false, true)},
    ((a, b, c)) => expect(Bool.xnor(a, b)) |> toEqual(c),
  )

  testAll(
    "implies",
    list{(true, true, true), (true, false, false), (false, true, true), (false, false, true)},
    ((a, b, c)) => expect(Bool.implies(a, b)) |> toEqual(c),
  )

  testAll(
    "eq",
    list{(true, true, true), (true, false, false), (false, true, false), (false, false, true)},
    ((a, b, c)) => expect(Bool.eq(a, b)) |> toEqual(c),
  )

  testAll(
    "compare",
    list{
      (true, true, #equal_to),
      (true, false, #greater_than),
      (false, true, #less_than),
      (false, false, #equal_to),
    },
    ((a, b, c)) => expect(Bool.compare(a, b)) |> toEqual(c),
  )

  testAll("show", list{(true, "true"), (false, "false")}, ((a, b)) =>
    expect(Bool.show(a)) |> toEqual(b)
  )
})

describe("Bool Conjuntive", () =>
  testAll(
    "append",
    list{(true, true, true), (true, false, false), (false, true, false), (false, false, false)},
    ((a, b, expected)) => expect(Bool.Conjunctive.Magma.append(a, b)) |> toEqual(expected),
  )
)

describe("Bool Disjunctive", () =>
  testAll(
    "append",
    list{(true, true, true), (true, false, true), (false, true, true), (false, false, false)},
    ((a, b, expected)) => expect(Bool.Disjunctive.Magma.append(a, b)) |> toEqual(expected),
  )
)

describe("Bool Bounded", () => {
  test("top", () => expect(Bool.Bounded.top) |> toEqual(true))

  test("bottom", () => expect(Bool.Bounded.bottom) |> toEqual(false))
})

describe("Bool Enum", () => {
  testAll("pred", list{(true, Some(false)), (false, None)}, ((input, expected)) =>
    expect(Bool.Enum.pred(input)) |> toEqual(expected)
  )

  testAll("succ", list{(true, None), (false, Some(true))}, ((input, expected)) =>
    expect(Bool.Enum.succ(input)) |> toEqual(expected)
  )
})

describe("Bool BoundedEnum", () => {
  test("cardinality", () => expect(Bool.BoundedEnum.cardinality) |> toEqual(2))

  testAll("fromEnum", list{(true, 1), (false, 0)}, ((input, expected)) =>
    expect(Bool.BoundedEnum.fromEnum(input)) |> toEqual(expected)
  )

  testAll("toEnum", list{(1, Some(true)), (0, Some(false)), (2, None), (-1, None)}, ((
    input,
    expected,
  )) => expect(Bool.BoundedEnum.toEnum(input)) |> toEqual(expected))
})
