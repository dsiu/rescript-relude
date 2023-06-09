open Jest
open Expect
open! Relude.Globals

type user = {
  id: string,
  name: string,
  age: int,
}

describe("Ord", () => {
  test("by", () => {
    let userCompare = Ord.by(user => user.name, String.compare)
    let user1 = {id: "2", name: "Andy", age: 99}
    let user2 = {id: "3", name: "Bob", age: 99}
    let user3 = {id: "1", name: "Clyde", age: 99}
    expect((
      userCompare(user1, user1),
      userCompare(user1, user2),
      userCompare(user1, user3),
      userCompare(user2, user1),
      userCompare(user2, user2),
      userCompare(user2, user3),
      userCompare(user3, user1),
      userCompare(user3, user2),
      userCompare(user3, user3),
    )) |> toEqual((
      #equal_to,
      #less_than,
      #less_than,
      #greater_than,
      #equal_to,
      #less_than,
      #greater_than,
      #greater_than,
      #equal_to,
    ))
  })

  testAll(
    "reverse",
    list{
      (list{1, 2, 3}, list{1, 2, 3}, list{3, 2, 1}),
      (list{3, 2, 1}, list{1, 2, 3}, list{3, 2, 1}),
      (list{2, 3, 1}, list{1, 2, 3}, list{3, 2, 1}),
      (list{1, 1, 1}, list{1, 1, 1}, list{1, 1, 1}),
    },
    ((input, expectedNormal, expectedReverse)) => {
      let compareNormal = Int.compare
      let compareReverse = compareNormal |> Ord.reverse
      let actualNormal = List.sortBy(compareNormal, input)
      let actualReverse = List.sortBy(compareReverse, input)
      expect((actualNormal, actualReverse)) |> toEqual((expectedNormal, expectedReverse))
    },
  )

  testAll("compareAsIntBy", list{("a", "a", 0), ("a", "b", -1), ("b", "a", 1)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.compareAsIntBy(String.compare, a, b)) |> toEqual(expected))

  testAll("compareAsIntBy", list{("a", "a", 0), ("a", "b", -1), ("b", "a", 1)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.compareAsInt(module(String.Ord), a, b)) |> toEqual(expected))

  testAll("minBy", list{("a", "a", "a"), ("a", "b", "a"), ("b", "a", "a")}, ((a, b, expected)) =>
    expect(Ord.minBy(String.compare, a, b)) |> toEqual(expected)
  )

  testAll("min", list{("a", "a", "a"), ("a", "b", "a"), ("b", "a", "a")}, ((a, b, expected)) =>
    expect(Ord.min(module(String.Ord), a, b)) |> toEqual(expected)
  )

  testAll("maxBy", list{("a", "a", "a"), ("a", "b", "b"), ("b", "a", "b")}, ((a, b, expected)) =>
    expect(Ord.maxBy(String.compare, a, b)) |> toEqual(expected)
  )

  testAll("max", list{("a", "a", "a"), ("a", "b", "b"), ("b", "a", "b")}, ((a, b, expected)) =>
    expect(Ord.max(module(String.Ord), a, b)) |> toEqual(expected)
  )

  testAll("lessThanBy", list{("a", "a", false), ("a", "b", true), ("b", "a", false)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.lessThanBy(String.compare, a, b)) |> toEqual(expected))

  testAll("lessThan", list{("a", "a", false), ("a", "b", true), ("b", "a", false)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.lessThan(module(String.Ord), a, b)) |> toEqual(expected))

  testAll("lessThanOrEqBy", list{("a", "a", true), ("a", "b", true), ("b", "a", false)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.lessThanOrEqBy(String.compare, a, b)) |> toEqual(expected))

  testAll("lessThanOrEq", list{("a", "a", true), ("a", "b", true), ("b", "a", false)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.lessThanOrEq(module(String.Ord), a, b)) |> toEqual(expected))

  testAll("greaterThanBy", list{("a", "a", false), ("a", "b", false), ("b", "a", true)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.greaterThanBy(String.compare, a, b)) |> toEqual(expected))

  testAll("greaterThan", list{("a", "a", false), ("a", "b", false), ("b", "a", true)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.greaterThan(module(String.Ord), a, b)) |> toEqual(expected))

  testAll("greaterThanOrEqBy", list{("a", "a", true), ("a", "b", false), ("b", "a", true)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.greaterThanOrEqBy(String.compare, a, b)) |> toEqual(expected))

  testAll("greaterThanOrEq", list{("a", "a", true), ("a", "b", false), ("b", "a", true)}, ((
    a,
    b,
    expected,
  )) => expect(Ord.greaterThanOrEq(module(String.Ord), a, b)) |> toEqual(expected))

  testAll(
    "clampBy",
    list{
      ("a", "b", "d", "b"),
      ("b", "b", "d", "b"),
      ("c", "b", "d", "c"),
      ("d", "b", "d", "d"),
      ("e", "b", "d", "d"),
    },
    ((input, min, max, expected)) =>
      expect(Ord.clampBy(String.compare, ~min, ~max, input)) |> toEqual(expected),
  )

  testAll(
    "clamp",
    list{
      ("a", "b", "d", "b"),
      ("b", "b", "d", "b"),
      ("c", "b", "d", "c"),
      ("d", "b", "d", "d"),
      ("e", "b", "d", "d"),
    },
    ((input, min, max, expected)) =>
      expect(Ord.clamp(module(String.Ord), ~min, ~max, input)) |> toEqual(expected),
  )

  testAll(
    "betweenBy",
    list{
      ("a", "b", "d", false),
      ("b", "b", "d", true),
      ("c", "b", "d", true),
      ("d", "b", "d", true),
      ("e", "b", "d", false),
    },
    ((input, min, max, expected)) =>
      expect(Ord.betweenBy(String.compare, ~min, ~max, input)) |> toEqual(expected),
  )

  testAll(
    "between",
    list{
      ("a", "b", "d", false),
      ("b", "b", "d", true),
      ("c", "b", "d", true),
      ("d", "b", "d", true),
      ("e", "b", "d", false),
    },
    ((input, min, max, expected)) =>
      expect(Ord.between(module(String.Ord), ~min, ~max, input)) |> toEqual(expected),
  )

  testAll("abs", list{(-2, 2), (-1, 1), (0, 0), (1, 1), (2, 2)}, ((input, expected)) =>
    expect(Ord.abs(module(Int.Ord), module(Int.Ring), input)) |> toEqual(expected)
  )

  testAll("signum", list{(-2, -1), (-1, -1), (0, 1), (1, 1), (2, 1)}, ((input, expected)) =>
    expect(Ord.signum(module(Int.Ord), module(Int.Ring), input)) |> toEqual(expected)
  )
})
