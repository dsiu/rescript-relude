open Jest
open Expect
open! Relude.Globals
open Relude_Tree

let testTree1 = Tree.make(1, list{Tree.pure(2), Tree.pure(3)})

let testTree2 = Tree.make(
  1,
  list{
    Tree.make(2, list{Tree.pure(21), Tree.pure(22)}),
    Tree.make(3, list{Tree.pure(31), Tree.pure(32)}),
    Tree.make(4, list{Tree.pure(41), Tree.pure(42)}),
  },
)

describe("Tree", () => {
  test("pure", () => expect(Tree.pure(42)) |> toEqual({value: 42, children: list{}}))

  test("singleton", () => expect(Tree.singleton(42)) |> toEqual({value: 42, children: list{}}))

  test("isSingleton", () =>
    expect((
      Tree.pure(42) |> Tree.isSingleton,
      Tree.make(42, list{Tree.pure(33)}) |> Tree.isSingleton,
    )) |> toEqual((true, false))
  )

  test("make", () =>
    expect(Tree.make(42, list{Tree.pure(1), Tree.pure(2)})) |> toEqual({
      value: 42,
      children: list{Tree.pure(1), Tree.pure(2)},
    })
  )

  test("unmake", () =>
    expect(testTree1 |> Tree.unmake) |> toEqual((1, list{Tree.pure(2), Tree.pure(3)}))
  )

  test("fill", () => {
    let m = Belt.Map.String.fromArray([("k", list{"a", "b"})])
    let lookup = key => m->Belt.Map.String.getWithDefault(key, list{})
    let actual = Tree.fill(lookup, "k")
    let expected = Tree.make("k", list{Tree.pure("a"), Tree.pure("b")})
    expect(actual) |> toEqual(expected)
  })

  test("getValue", () => expect(testTree1 |> Tree.getValue) |> toEqual(1))

  test("setValue", () =>
    expect(testTree1 |> Tree.setValue(100)) |> toEqual({
      value: 100,
      children: list{Tree.pure(2), Tree.pure(3)},
    })
  )

  test("modifyValue", () =>
    expect(testTree1 |> Tree.modifyValue(a => a + 5)) |> toEqual({
      value: 6,
      children: list{Tree.pure(2), Tree.pure(3)},
    })
  )

  test("getChildren", () =>
    expect(testTree1 |> getChildren) |> toEqual(list{Tree.pure(2), Tree.pure(3)})
  )

  test("getChildAt", () =>
    expect((testTree1 |> Tree.getChildAt(1), testTree1 |> Tree.getChildAt(2))) |> toEqual((
      Some(Tree.pure(3)),
      None,
    ))
  )

  test("setChildren", () =>
    expect(testTree1 |> setChildren(list{Tree.pure(42), Tree.pure(43)})) |> toEqual({
      value: 1,
      children: list{Tree.pure(42), Tree.pure(43)},
    })
  )

  test("modifyChildren", () =>
    expect(
      testTree1 |> modifyChildren(children =>
        children |> List.map(child => child |> Tree.modifyValue(a => a * 100))
      ),
    ) |> toEqual({value: 1, children: list{Tree.pure(200), Tree.pure(300)}})
  )

  test("prependChild", () =>
    expect(testTree1 |> Tree.prependChild(~child=Tree.pure(0))) |> toEqual({
      value: 1,
      children: list{Tree.pure(0), Tree.pure(2), Tree.pure(3)},
    })
  )

  test("appendChild", () =>
    expect(testTree1 |> Tree.appendChild(~child=Tree.pure(0))) |> toEqual({
      value: 1,
      children: list{Tree.pure(2), Tree.pure(3), Tree.pure(0)},
    })
  )

  test("prependChildren", () =>
    expect(testTree1 |> Tree.prependChildren(list{Tree.pure(-1), Tree.pure(0)})) |> toEqual({
      value: 1,
      children: list{Tree.pure(-1), Tree.pure(0), Tree.pure(2), Tree.pure(3)},
    })
  )

  test("appendChildren", () =>
    expect(testTree1 |> Tree.appendChildren(list{Tree.pure(-1), Tree.pure(0)})) |> toEqual({
      value: 1,
      children: list{Tree.pure(2), Tree.pure(3), Tree.pure(-1), Tree.pure(0)},
    })
  )

  test("toNonEmptyList", () =>
    expect(testTree2 |> Tree.toNonEmptyList) |> toEqual(
      NonEmpty.List.make(1, list{2, 21, 22, 3, 31, 32, 4, 41, 42}),
    )
  )

  test("toNonEmptyArray", () =>
    expect(testTree2 |> Tree.toNonEmptyArray) |> toEqual(
      NonEmpty.Array.make(1, [2, 21, 22, 3, 31, 32, 4, 41, 42]),
    )
  )

  test("zipWith", () => {
    let tree1 = Tree.make(1, list{Tree.pure(2), Tree.pure(3), Tree.pure(4)})
    let tree2 = Tree.make(100, list{Tree.pure(200), Tree.pure(300)})
    let actual = Tree.zipWith(\"+", tree1, tree2)
    let expected = Tree.make(101, list{Tree.pure(202), Tree.pure(303)})
    expect(actual) |> toEqual(expected)
  })

  test("zip", () => {
    let tree1 = Tree.make(1, list{Tree.pure(2), Tree.pure(3), Tree.pure(4)})
    let tree2 = Tree.make(100, list{Tree.pure(200), Tree.pure(300)})
    let actual = Tree.zip(tree1, tree2)
    let expected = Tree.make((1, 100), list{Tree.pure((2, 200)), Tree.pure((3, 300))})
    expect(actual) |> toEqual(expected)
  })

  test("map", () =>
    expect(testTree1 |> Tree.map(string_of_int)) |> toEqual(
      Tree.make("1", list{Tree.pure("2"), Tree.pure("3")}),
    )
  )

  test("apply", () => {
    let testTreeF = Tree.make(a => a * 10, list{Tree.pure(a => a * 100), Tree.pure(a => a * 1000)})
    let actual = testTree1 |> Tree.apply(testTreeF)
    // Not sure if this is the expected result for apply on a tree, but sure
    let expected = Tree.make(
      10,
      list{
        Tree.make(20, list{}),
        Tree.make(30, list{}),
        Tree.make(100, list{Tree.pure(200), Tree.pure(300)}),
        Tree.make(1000, list{Tree.pure(2000), Tree.pure(3000)}),
      },
    )
    expect(actual) |> toEqual(expected)
  })

  test("flatMap", () => {
    let f = a =>
      Tree.make(
        string_of_int(a * 10),
        list{Tree.pure(string_of_int(a * 100)), Tree.pure(string_of_int(a * 1000))},
      )
    let actual = testTree1 |> Tree.flatMap(f)
    let expected = Tree.make(
      "10",
      list{
        Tree.pure("100"),
        Tree.pure("1000"),
        Tree.make("20", list{Tree.pure("200"), Tree.pure("2000")}),
        Tree.make("30", list{Tree.pure("300"), Tree.pure("3000")}),
      },
    )
    expect(actual) |> toEqual(expected)
  })

  test("extend", () => {
    let f = tree => tree |> Tree.foldLeft(\"+", 0)
    let actual = Tree.extend(f, testTree2)
    let expected = Tree.make(
      199,
      list{
        Tree.make(45, list{Tree.pure(21), Tree.pure(22)}),
        Tree.make(66, list{Tree.pure(31), Tree.pure(32)}),
        Tree.make(87, list{Tree.pure(41), Tree.pure(42)}),
      },
    )
    expect(actual) |> toEqual(expected)
  })

  test("foldLeft", () =>
    expect(
      testTree2 |> Tree.foldLeft((acc, value) => acc |> List.append(value), list{}),
    ) |> toEqual(list{21, 22, 2, 31, 32, 3, 41, 42, 4, 1})
  )

  test("foldRight", () =>
    expect(
      testTree2 |> Tree.foldRight((value, acc) => acc |> List.append(value), list{}),
    ) |> toEqual(list{42, 41, 4, 32, 31, 3, 22, 21, 2, 1})
  )

  test("Fold_Map", () => {
    module FoldMap = Tree.Foldable.Fold_Map(String.Monoid)
    let actual = FoldMap.fold_map(a => string_of_int(a) ++ "-", testTree2)
    let expected = "21-22-2-31-32-3-41-42-4-1-"
    expect(actual) |> toEqual(expected)
  })

  test("Fold_Map_Any", () => {
    module FoldMapAny = Tree.Foldable.Fold_Map_Any(List.MonoidAny)
    let actual = FoldMapAny.fold_map(a => list{a * 10, a * 20}, testTree1)
    let expected = list{20, 40, 30, 60, 10, 20}
    expect(actual) |> toEqual(expected)
  })

  test("Fold_Map_Plus", () => {
    module FoldMapPlus = Tree.Foldable.Fold_Map_Plus(Option.Plus)
    let actual = FoldMapPlus.fold_map(a =>
      if mod(a, 2) == 0 {
        Some(a)
      } else {
        None
      }
    , testTree1)
    let expected = Some(2)
    expect(actual) |> toEqual(expected)
  })

  test("unfold", () => {
    let f = a =>
      if a < 4 {
        Some((a, a + 1))
      } else {
        None
      }
    let actual = Tree.unfold(f, 0)
    let expected = Tree.make(
      0,
      list{Tree.make(1, list{Tree.make(2, list{Tree.make(3, list{Tree.make(4, list{})})})})},
    )
    expect(actual) |> toEqual(expected)
  })

  test("traverse", () => {
    module TreeOption = Tree.WithApplicative(Relude_Option.Applicative)
    let actual = TreeOption.Traversable.traverse(
      a => Some(a + 100),
      Tree.make(1, list{Tree.pure(2), Tree.pure(3)}),
    )
    let expected = Some(Tree.make(101, list{Tree.pure(102), Tree.pure(103)}))
    expect(actual) |> toEqual(expected)
  })

  test("sequence", () => {
    module TreeOption = Tree.WithApplicative(Relude_Option.Applicative)
    let actual = TreeOption.Traversable.sequence(
      Tree.make(Some(1), list{Tree.pure(Some(2)), Tree.pure(Some(3))}),
    )
    let expected = Some(Tree.make(1, list{Tree.pure(2), Tree.pure(3)}))
    expect(actual) |> toEqual(expected)
  })

  test("filter", () => {
    let actual = testTree2 |> Tree.filter(a => a < 30)
    let expected = Tree.make(
      1,
      list{Tree.make(2, list{Tree.pure(21), Tree.pure(22)}), Tree.pure(3), Tree.pure(4)},
    )
    expect(actual) |> toEqual(Some(expected))
  })

  test("showBy", () =>
    expect(testTree1 |> showBy(string_of_int)) |> toEqual("Tree 1 [Tree 2 [], Tree 3 []]")
  )

  test("Show", () => {
    module Show = Tree.Show(Relude_Int.Show)
    expect(testTree1 |> Show.show) |> toEqual("Tree 1 [Tree 2 [], Tree 3 []]")
  })

  test("showPrettyBy", () => {
    let tree = Tree.make(
      1,
      list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.make(22, list{})},
        ),
        Tree.make(3, list{Tree.pure(31), Tree.make(32, list{Tree.pure(321), Tree.pure(322)})}),
      },
    )
    let actual = tree |> showPrettyBy(string_of_int)
    let expected = "1
|- 2
   |- 21
      |- 211
      |- 212
   |- 22
|- 3
   |- 31
   |- 32
      |- 321
      |- 322
"
    expect(actual) |> toEqual(expected)
  })

  test("eqBy", () => {
    let eq1 = Tree.eqBy(Int.eq, testTree1, testTree1)
    let eq2 = Tree.eqBy(Int.eq, testTree1, testTree2)
    let eq3 = Tree.eqBy(Int.eq, testTree1, Tree.pure(42))
    expect((eq1, eq2, eq3)) |> toEqual((true, false, false))
  })

  test("Eq", () => {
    module Eq = Tree.Eq(Int.Eq)
    let eq1 = Eq.eq(testTree1, testTree1)
    let eq2 = Eq.eq(testTree1, testTree2)
    let eq3 = Eq.eq(testTree1, Tree.pure(42))
    expect((eq1, eq2, eq3)) |> toEqual((true, false, false))
  })
})
