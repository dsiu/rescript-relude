open Jest
open Expect

module Int = Relude_Int
module List = Relude.List
module IO = Relude.IO

describe("List", () => {
  test("length empty list", () => expect(List.length(list{})) |> toEqual(0))

  test("length non-empty list", () => expect(List.length(list{1, 2, 3})) |> toEqual(3))

  test("isEmpty is true for empty list", () => expect(List.isEmpty(list{})) |> toBe(true))

  test("isEmpty is false for non-empty list", () => expect(List.isEmpty(list{1})) |> toBe(false))

  test("isNotEmpty is false for empty list", () => expect(List.isNotEmpty(list{})) |> toBe(false))

  test("isNotEmpty is true for non-empty list", () =>
    expect(List.isNotEmpty(list{1})) |> toBe(true)
  )

  test("empty is []", () => expect(List.empty) |> toEqual(list{}))

  test("pure creates a one-item list", () => expect(List.pure(123)) |> toEqual(list{123}))

  test("repeat creates a list of n items", () =>
    expect(List.repeat(3, "a")) |> toEqual(list{"a", "a", "a"})
  )

  test("repeat (negative count is empty)", () => expect(List.repeat(-1, "a")) |> toEqual(list{}))

  test("makeWithIndex creates a list of n items using f", () =>
    expect(List.makeWithIndex(3, i => i + 2)) |> toEqual(list{2, 3, 4})
  )

  test("concat", () => expect(List.concat(list{1, 2}, list{3, 4})) |> toEqual(list{1, 2, 3, 4}))

  test("concatNamed", () =>
    expect(List.concatNamed(~prefix=list{1, 2}, list{3, 4})) |> toEqual(list{1, 2, 3, 4})
  )

  test("guard true", () => expect(List.guard(true, list{1, 2, 3})) |> toEqual(list{1, 2, 3}))

  test("guard false", () => expect(List.guard(false, list{1, 2, 3})) |> toEqual(list{}))

  test("power", () =>
    expect(List.power(list{1, 2, 3}, 3)) |> toEqual(list{1, 2, 3, 1, 2, 3, 1, 2, 3})
  )

  test("cons", () => expect(List.cons(1, list{2, 3})) |> toEqual(list{1, 2, 3}))

  test("uncons empty list", () => expect(List.uncons(list{})) |> toEqual(None))

  test("uncons single item list", () => expect(List.uncons(list{1})) |> toEqual(Some((1, list{}))))

  test("uncons multi item list", () =>
    expect(List.uncons(list{1, 2, 3})) |> toEqual(Some((1, list{2, 3})))
  )

  test("prepend", () => expect(List.prepend(1, list{2, 3})) |> toEqual(list{1, 2, 3}))

  test("append", () => expect(List.append(1, list{2, 3})) |> toEqual(list{2, 3, 1}))

  test("appendOption Some appends element", () =>
    expect(List.appendOption(Some(1), list{2, 3})) |> toEqual(list{2, 3, 1})
  )

  test("appendOption None no-ops", () =>
    expect(List.appendOption(None, list{2, 3})) |> toEqual(list{2, 3})
  )

  test("consOption Some conss element", () =>
    expect(List.consOption(Some(1), list{2, 3})) |> toEqual(list{1, 2, 3})
  )

  test("consOption None no-ops", () =>
    expect(List.consOption(None, list{2, 3})) |> toEqual(list{2, 3})
  )

  test("foldLeft", () =>
    expect(
      List.foldLeft((acc, curr) => List.append(curr, acc), list{}, list{1, 2, 3, 4, 5}),
    ) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("foldRight", () =>
    expect(
      List.foldRight((curr, acc) => List.append(curr, acc), list{}, list{1, 2, 3, 4, 5}),
    ) |> toEqual(list{5, 4, 3, 2, 1})
  )

  test("unfold", () => expect(List.unfold(x =>
        if x > 5 {
          None
        } else {
          Some((x, x + 1))
        }
      , 0)) |> toEqual(list{0, 1, 2, 3, 4, 5}))

  test("scanLeft", () =>
    expect(
      List.scanLeft((acc, curr) => List.append(curr, acc), list{}, list{1, 2, 3, 4, 5}),
    ) |> toEqual(list{list{1}, list{1, 2}, list{1, 2, 3}, list{1, 2, 3, 4}, list{1, 2, 3, 4, 5}})
  )

  test("scanRight", () =>
    expect(
      List.scanRight((curr, acc) => List.append(curr, acc), list{}, list{1, 2, 3, 4, 5}),
    ) |> toEqual(list{list{5, 4, 3, 2, 1}, list{5, 4, 3, 2}, list{5, 4, 3}, list{5, 4}, list{5}})
  )

  test("at empty list", () => expect(List.at(0, list{})) |> toEqual(None))

  test("at success", () => expect(List.at(2, list{0, 10, 20, 30})) |> toEqual(Some(20)))

  test("at failure", () => expect(List.at(10, list{0, 10, 20, 30})) |> toEqual(None))

  test("head empty list", () => expect(List.head(list{})) |> toEqual(None))

  test("head single item list", () => expect(List.head(list{1})) |> toEqual(Some(1)))

  test("head multi-item list", () => expect(List.head(list{1, 2, 3})) |> toEqual(Some(1)))

  test("tail empty list", () => expect(List.tail(list{})) |> toEqual(None))

  test("tail single item list", () => expect(List.tail(list{1})) |> toEqual(Some(list{})))

  test("tail multi-item list", () => expect(List.tail(list{1, 2, 3})) |> toEqual(Some(list{2, 3})))

  test("tailOrEmpty empty list", () => expect(List.tailOrEmpty(list{})) |> toEqual(list{}))

  test("tailOrEmpty single item list", () => expect(List.tailOrEmpty(list{1})) |> toEqual(list{}))

  test("tailOrEmpty multi-item list", () =>
    expect(List.tailOrEmpty(list{1, 2, 3})) |> toEqual(list{2, 3})
  )

  test("init empty list", () => expect(List.init(list{})) |> toEqual(None))

  test("init single item list", () => expect(List.init(list{1})) |> toEqual(Some(list{})))

  test("init multi item list", () =>
    expect(List.init(list{1, 2, 3, 4})) |> toEqual(Some(list{1, 2, 3}))
  )

  test("initOrEmpty empty list", () => expect(List.initOrEmpty(list{})) |> toEqual(list{}))

  test("initOrEmpty single item list", () => expect(List.initOrEmpty(list{1})) |> toEqual(list{}))

  test("initOrEmpty multi item list", () =>
    expect(List.initOrEmpty(list{1, 2, 3, 4})) |> toEqual(list{1, 2, 3})
  )

  test("last empty list", () => expect(List.last(list{})) |> toEqual(None))

  test("last single item list", () => expect(List.last(list{1})) |> toEqual(Some(1)))

  test("last multi item list", () => expect(List.last(list{1, 2, 3, 4})) |> toEqual(Some(4)))

  test("take zero from empty list", () => expect(List.take(0, list{})) |> toEqual(list{}))

  test("take non-zero from empty list", () => expect(List.take(2, list{})) |> toEqual(list{}))

  test("take non-zero from short list", () => expect(List.take(2, list{1})) |> toEqual(list{1}))

  test("take non-zero from equal list", () =>
    expect(List.take(2, list{1, 2})) |> toEqual(list{1, 2})
  )

  test("take non-zero from long list", () =>
    expect(List.take(2, list{1, 2, 3})) |> toEqual(list{1, 2})
  )

  test("takeExactly negative from list", () =>
    expect(List.takeExactly(-2, list{})) |> toEqual(None)
  )

  test("takeExactly zero from empty list", () =>
    expect(List.takeExactly(0, list{})) |> toEqual(Some(list{}))
  )

  test("takeExactly non-zero from empty list", () =>
    expect(List.takeExactly(2, list{})) |> toEqual(None)
  )

  test("takeExactly non-zero from short list", () =>
    expect(List.takeExactly(2, list{1})) |> toEqual(None)
  )

  test("takeExactly non-zero from equal list", () =>
    expect(List.takeExactly(2, list{1, 2})) |> toEqual(Some(list{1, 2}))
  )

  test("takeExactly non-zero from long list", () =>
    expect(List.takeExactly(2, list{1, 2, 3})) |> toEqual(Some(list{1, 2}))
  )

  test("takeWhile empty list", () => expect(List.takeWhile(a => a < 2, list{})) |> toEqual(list{}))

  test("takeWhile list", () =>
    expect(List.takeWhile(a => a < 2, list{0, 1, 2, 3})) |> toEqual(list{0, 1})
  )

  test("takeWhile list condition never true", () =>
    expect(List.takeWhile(a => a < 0, list{0, 1, 2, 3})) |> toEqual(list{})
  )

  test("takeWhile list condition never false", () =>
    expect(List.takeWhile(a => a < 10, list{0, 1, 2, 3})) |> toEqual(list{0, 1, 2, 3})
  )

  test("drop negative from empty list", () => expect(List.drop(-2, list{})) |> toEqual(list{}))

  test("drop negative from nonempty list", () =>
    expect(List.drop(-2, list{1, 2})) |> toEqual(list{1, 2})
  )

  test("drop zero from empty list", () => expect(List.drop(0, list{})) |> toEqual(list{}))

  test("drop zero from nonempty list", () =>
    expect(List.drop(0, list{1, 2})) |> toEqual(list{1, 2})
  )

  test("drop some from short list ", () => expect(List.drop(1, list{1, 2})) |> toEqual(list{2}))

  test("drop some from equal list ", () => expect(List.drop(2, list{1, 2})) |> toEqual(list{}))

  test("drop some from long list ", () =>
    expect(List.drop(3, list{1, 2, 3, 4, 5})) |> toEqual(list{4, 5})
  )

  test("drop more from long list ", () => expect(List.drop(5, list{1, 2, 3, 4})) |> toEqual(list{}))

  test("dropWhile empty list", () => expect(List.dropWhile(a => a < 2, list{})) |> toEqual(list{}))

  test("dropExactly zero from empty list", () =>
    expect(List.dropExactly(0, list{})) |> toEqual(Some(list{}))
  )

  test("dropExactly some from short list ", () =>
    expect(List.dropExactly(1, list{1, 2})) |> toEqual(Some(list{2}))
  )

  test("dropExactly some from equal list ", () =>
    expect(List.dropExactly(2, list{1, 2})) |> toEqual(Some(list{}))
  )

  test("dropExactly some from long list ", () =>
    expect(List.dropExactly(2, list{1, 2, 3, 4})) |> toEqual(Some(list{3, 4}))
  )

  test("dropExactly more from long list ", () =>
    expect(List.dropExactly(5, list{1, 2, 3, 4})) |> toEqual(None)
  )

  test("dropWhile list", () =>
    expect(List.dropWhile(a => a < 2, list{0, 1, 2, 1, 3})) |> toEqual(list{2, 1, 3})
  )

  test("dropWhile list condition never true", () =>
    expect(List.dropWhile(a => a < 0, list{0, 1, 2, 3})) |> toEqual(list{0, 1, 2, 3})
  )

  test("dropWhile list condition never false", () =>
    expect(List.dropWhile(a => a < 10, list{0, 1, 2, 3})) |> toEqual(list{})
  )

  test("filter", () =>
    expect(
      List.filter(i => mod(i, 2) == 0, list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 10}),
    ) |> toEqual(list{2, 4, 6, 8, 10, 10})
  )

  test("filterWithIndex", () =>
    expect(
      List.filterWithIndex((v, i) => mod(i, 2) == 0 || v == 3, list{1, 3, 3, 4, 5, 6}),
    ) |> toEqual(list{1, 3, 3, 5})
  )

  test("filterNot", () =>
    expect(List.filterNot(x => mod(x, 2) == 0, list{1, 2, 3, 4})) |> toEqual(list{1, 3})
  )

  test("filterNotWithIndex", () =>
    expect(List.filterNotWithIndex((_, idx) => idx > 2, list{0, 0, 0, 0})) |> toEqual(list{0, 0, 0})
  )

  test("find not found", () => expect(List.find(a => a == 3, list{0, 1, 2})) |> toEqual(None))

  test("find found", () => expect(List.find(a => a == 2, list{0, 1, 2, 3, 4})) |> toEqual(Some(2)))

  test("findWithIndex", () =>
    expect(List.findWithIndex((a, i) => a == 3 || i == 2, list{0, 1, 2, 3})) |> toEqual(Some(2))
  )

  test("partition", () =>
    expect(List.partition(a => mod(a, 2) == 0, list{0, 1, 2, 3, 4, 5})) |> toEqual((
      list{0, 2, 4},
      list{1, 3, 5},
    ))
  )

  test("splitAt", () =>
    expect(List.splitAt(3, list{0, 1, 2, 3, 4, 5})) |> toEqual(Some((list{0, 1, 2}, list{3, 4, 5})))
  )

  test("prependToAll", () =>
    expect(List.prependToAll(0, list{0, 1, 2})) |> toEqual(list{0, 0, 0, 1, 0, 2})
  )

  test("intersperse", () =>
    expect(List.intersperse(",", list{"a", "b", "c"})) |> toEqual(list{"a", ",", "b", ",", "c"})
  )

  test("intersperse is tail recursive", () =>
    expect({
      open List
      repeat(20000, 0) |> intersperse(1) |> length
    }) |> toEqual(39999)
  )

  test("replicate", () =>
    expect(List.replicate(3, list{"a", "b", "c"})) |> toEqual(list{
      "a",
      "b",
      "c",
      "a",
      "b",
      "c",
      "a",
      "b",
      "c",
    })
  )

  test("replicate once", () => expect(List.replicate(1, list{"foo"})) |> toEqual(list{"foo"}))

  test("replicate zero", () => expect(List.replicate(0, list{"none"})) |> toEqual(list{}))

  test("replicate negative", () => expect(List.replicate(-1, list{0})) |> toEqual(list{}))

  test("replicate empty list", () => expect(List.replicate(10, list{})) |> toEqual(list{}))

  test("zip same length lists", () =>
    expect(List.zip(list{1, 2, 3}, list{"4", "5", "6"})) |> toEqual(list{
      (1, "4"),
      (2, "5"),
      (3, "6"),
    })
  )

  test("zip different length lists", () =>
    expect(List.zip(list{1, 2, 3}, list{"4", "5"})) |> toEqual(list{(1, "4"), (2, "5")})
  )

  test("zipWith", () =>
    expect(List.zipWith((a, b) => a + b, list{1, 2, 3}, list{4, 5, 6})) |> toEqual(list{5, 7, 9})
  )

  test("zipWithIndex", () =>
    expect(List.zipWithIndex(list{"a", "b", "c"})) |> toEqual(list{("a", 0), ("b", 1), ("c", 2)})
  )

  test("unzip", () =>
    expect(List.unzip(list{(1, "a"), (2, "b"), (3, "c")})) |> toEqual((
      list{1, 2, 3},
      list{"a", "b", "c"},
    ))
  )

  test("sortWithInt", () =>
    expect(List.sortWithInt(Int.compareAsInt, list{2, 0, 1, 3, 5, -1})) |> toEqual(list{
      -1,
      0,
      1,
      2,
      3,
      5,
    })
  )

  test("sortBy", () =>
    expect(List.sortBy(Int.compare, list{2, 0, 1, 3, 5, -1})) |> toEqual(list{-1, 0, 1, 2, 3, 5})
  )

  test("sort", () =>
    expect(List.sort(module(Int.Ord), list{2, 0, 1, 3, 5, -1})) |> toEqual(list{-1, 0, 1, 2, 3, 5})
  )

  test("reverse", () => expect(List.reverse(list{1, 2, 3, 4, 5})) |> toEqual(list{5, 4, 3, 2, 1}))

  test("contains false", () =>
    expect(List.containsBy(Int.eq, 10, list{0, 1, 2, 3, 4})) |> toEqual(false)
  )

  test("contains true", () =>
    expect(List.containsBy(Int.eq, 3, list{0, 1, 2, 3, 4})) |> toEqual(true)
  )

  test("indexOfBy failure", () =>
    expect(List.indexOfBy(Int.eq, 500, list{0, 10, 20, 30, 40})) |> toEqual(None)
  )

  test("indexOfBy success", () =>
    expect(List.indexOfBy(Int.eq, 30, list{0, 10, 20, 30, 40})) |> toEqual(Some(3))
  )

  test("indexOf success", () =>
    expect(List.indexOf(module(Int.Eq), 30, list{0, 10, 20, 30, 40})) |> toEqual(Some(3))
  )

  test("indexOf failure", () =>
    expect(List.indexOf(module(Int.Eq), 500, list{0, 10, 20, 30, 40})) |> toEqual(None)
  )

  test("minBy empty", () => expect(List.minBy(Int.Ord.compare, list{})) |> toEqual(None))

  test("minBy one", () => expect(List.minBy(Int.Ord.compare, list{0})) |> toEqual(Some(0)))

  test("minBy many", () => expect(List.minBy(Int.Ord.compare, list{3, 1, 2})) |> toEqual(Some(1)))

  test("min empty", () => expect(List.min(module(Relude_String.Ord), list{})) |> toEqual(None))

  test("min many", () =>
    expect(List.min(module(Relude_String.Ord), list{"b", "a", "c"})) |> toEqual(Some("a"))
  )

  test("maxBy empty", () => expect(List.maxBy(Int.Ord.compare, list{})) |> toEqual(None))

  test("maxBy one", () => expect(List.maxBy(Int.Ord.compare, list{0})) |> toEqual(Some(0)))

  test("maxBy many", () => expect(List.maxBy(Int.Ord.compare, list{3, 1, 2})) |> toEqual(Some(3)))

  test("max empty", () => expect(List.max(module(Int.Ord), list{})) |> toEqual(None))

  test("max many", () => expect(List.max(module(Int.Ord), list{3, 1, 2})) |> toEqual(Some(3)))

  test("any empty", () => expect(List.any(a => a > 2, list{})) |> toEqual(false))

  test("any true", () => expect(List.any(a => a > 2, list{0, 1, 2, 3})) |> toEqual(true))

  test("any false", () => expect(List.any(a => a > 10, list{0, 1, 2, 3})) |> toEqual(false))

  test("all empty", () => expect(List.all(a => a > 2, list{})) |> toEqual(true))

  test("all true", () => expect(List.all(a => a > -1, list{0, 1, 2, 3})) |> toEqual(true))

  test("all false", () => expect(List.all(a => a < 3, list{0, 1, 2, 3})) |> toEqual(false))

  test("removeFirstBy empty", () =>
    expect(List.removeFirstBy(Int.eq, 0, list{})) |> toEqual(list{})
  )

  test("removeFirstBy single match", () =>
    expect(List.removeFirstBy(Int.eq, 0, list{0})) |> toEqual(list{})
  )

  test("removeFirstBy single not a match", () =>
    expect(List.removeFirstBy(Int.eq, 0, list{1})) |> toEqual(list{1})
  )

  test("removeFirstBy one match at beginning", () =>
    expect(List.removeFirstBy(Int.eq, 0, list{0, 1, 2})) |> toEqual(list{1, 2})
  )

  test("removeFirstBy one match at end", () =>
    expect(List.removeFirstBy(Int.eq, 2, list{0, 1, 2})) |> toEqual(list{0, 1})
  )

  test("removeFirstBy many matches", () =>
    expect(List.removeFirstBy(Int.eq, 0, list{1, 0, 2, 0})) |> toEqual(list{1, 2, 0})
  )

  test("removeEachBy empty", () => expect(List.removeEachBy(Int.eq, 0, list{})) |> toEqual(list{}))

  test("removeEachBy single match", () =>
    expect(List.removeEachBy(Int.eq, 0, list{0})) |> toEqual(list{})
  )

  test("removeEachBy single not a match", () =>
    expect(List.removeEachBy(Int.eq, 0, list{1})) |> toEqual(list{1})
  )

  test("removeEachBy many matches removed", () =>
    expect(List.removeEachBy(Int.eq, 0, list{0, 2, 0, 4, 0})) |> toEqual(list{2, 4})
  )

  test("replaceAt", () =>
    expect(List.replaceAt(2, 100, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 100, 4, 5})
  )

  test("insertAt first", () =>
    expect(List.insertAt(0, 100, list{1, 2, 3, 4, 5})) |> toEqual(list{100, 1, 2, 3, 4, 5})
  )

  test("insertAt within range", () =>
    expect(List.insertAt(2, 100, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 100, 3, 4, 5})
  )

  test("insertAt last", () =>
    expect(List.insertAt(5, 100, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 3, 4, 5, 100})
  )

  test("insertAt out of range", () =>
    expect(List.insertAt(6, 100, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("updateAt within range", () =>
    expect(List.updateAt(2, x => x + 1, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 4, 4, 5})
  )

  test("updateAt out of range", () =>
    expect(List.updateAt(6, x => x + 1, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("swapAt within range", () =>
    expect(List.swapAt(0, 4, list{1, 2, 3, 4, 5})) |> toEqual(list{5, 2, 3, 4, 1})
  )

  test("swapAt out of range", () =>
    expect(List.swapAt(0, 5, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("removeAt within range", () =>
    expect(List.removeAt(1, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 3, 4, 5})
  )

  test("removeAt out of range", () =>
    expect(List.removeAt(5, list{1, 2, 3, 4, 5})) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("distinctBy", () =>
    expect(List.distinctBy(Int.eq, list{6, 1, 1, 2, 1, 3, 2, 3, 2, 4, 5, 5})) |> toEqual(list{
      6,
      1,
      2,
      3,
      4,
      5,
    })
  )

  test("map", () => expect(List.map(a => a + 2, list{1, 2, 3})) |> toEqual(list{3, 4, 5}))

  test("apply", () =>
    expect(List.apply(list{a => a + 2, a => a * 3}, list{1, 2, 3})) |> toEqual(list{
      3,
      4,
      5,
      3,
      6,
      9,
    })
  )

  test("bind", () =>
    expect(List.bind(list{1, 2, 3}, a => list{a, a})) |> toEqual(list{1, 1, 2, 2, 3, 3})
  )

  test("flatten", () =>
    expect(List.flatten(list{list{1, 2, 3}, list{4, 5}})) |> toEqual(list{1, 2, 3, 4, 5})
  )

  test("fromArray", () => expect(List.fromArray([1, 2, 3])) |> toEqual(list{1, 2, 3}))

  test("toArray", () => expect(List.toArray(list{1, 2, 3})) |> toEqual([1, 2, 3]))

  test("eqBy returns true if list items are equal", () =>
    expect(List.eqBy(Int.eq, list{1, 2, 3}, list{1, 2, 3})) |> toBe(true)
  )

  test("eqBy returns false if list items are not equal", () =>
    expect(List.eqBy(Int.eq, list{1, 2, 3}, list{1, 2, 4})) |> toBe(false)
  )

  test("eqBy returns false if lists are of different sizes", () =>
    expect(List.eqBy(Int.eq, list{1}, list{1, 2})) |> toBe(false)
  )

  test("eq returns true if list items are equal", () =>
    expect(List.eq(module(Int.Eq), list{1, 2, 3}, list{1, 2, 3})) |> toBe(true)
  )

  test("eq returns false if list items are not equal", () =>
    expect(List.eq(module(Int.Eq), list{1, 2, 3}, list{1, 2, 4})) |> toBe(false)
  )

  test("mapOption keep all", () =>
    expect(List.mapOption(v => Some(string_of_int(v)), list{1, 2, 3})) |> toEqual(list{
      "1",
      "2",
      "3",
    })
  )

  test("mapOption keep none", () =>
    expect(List.mapOption(_ => None, list{1, 2, 3})) |> toEqual(list{})
  )

  test("mapOption keep int", () =>
    expect(List.mapOption(Relude_String.toInt, list{"1", "a", "2", "", "3"})) |> toEqual(list{
      1,
      2,
      3,
    })
  )

  test("showBy", () => expect(List.showBy(string_of_int, list{1, 2, 3})) |> toEqual("[1, 2, 3]"))

  test("void", () => expect(List.void(list{1, 2, 3})) |> toEqual(list{(), (), ()}))

  test("flap", () =>
    expect(List.flap(list{a => a + 1, a => a + 2, a => a + 3}, 5)) |> toEqual(list{6, 7, 8})
  )

  test("map2", () =>
    expect(List.map2((a, b) => a + b, list{1, 2}, list{3, 4})) |> toEqual(list{4, 5, 5, 6})
  )

  test("map3", () =>
    expect(List.map3((a, b, c) => a + b + c, list{1, 2}, list{3, 4}, list{10, 20})) |> toEqual(list{
      14,
      24,
      15,
      25,
      15,
      25,
      16,
      26,
    })
  )

  test("map4", () =>
    expect(
      List.map4(
        (a, b, c, d) => a + b + c + d,
        list{1, 2},
        list{3, 4},
        list{10, 20},
        list{100, 200},
      ),
    ) |> toEqual(list{
      114,
      214,
      124,
      224,
      115,
      215,
      125,
      225,
      115,
      215,
      125,
      225,
      116,
      216,
      126,
      226,
    })
  )

  test("chunk", () =>
    expect(
      list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16} |> List.chunk(3),
    ) |> toEqual(list{
      list{1, 2, 3},
      list{4, 5, 6},
      list{7, 8, 9},
      list{10, 11, 12},
      list{13, 14, 15},
      list{16},
    })
  )

  test("List.Float.sum empty", () => expect(List.Float.sum(list{})) |> toEqual(0.))

  test("List.Float.sum one", () => expect(List.Float.sum(list{1.})) |> toEqual(1.))

  test("List.Float.sum many", () => expect(List.Float.sum(list{1., 3., 5.})) |> toEqual(9.))

  test("countBy empty", () => expect(List.countBy(_ => true, list{})) |> toEqual(0))

  test("countBy", () => expect(List.countBy(v => v < 10, list{1, 12, 3, 20, 4})) |> toEqual(3))

  test("String.eq empties true", () => expect(List.String.eq(list{}, list{})) |> toEqual(true))

  test("String.eq same values true", () =>
    expect(List.String.eq(list{"a", "b"}, list{"a", "b"})) |> toEqual(true)
  )

  test("String.eq different values false", () =>
    expect(List.String.eq(list{"a", "c"}, list{"a", "b"})) |> toEqual(false)
  )

  test("String.eq different size false", () =>
    expect(List.String.eq(list{"a", "c"}, list{"a"})) |> toEqual(false)
  )

  test("String.min empty", () => expect(List.String.min(list{})) |> toEqual(None))

  test("String.min non-empty", () =>
    expect(List.String.min(list{"foo", "bar"})) |> toEqual(Some("bar"))
  )

  test("String.max non-empty", () =>
    expect(List.String.max(list{"b", "a", "c"})) |> toEqual(Some("c"))
  )

  test("String.contains empty", () => expect(List.String.contains("a", list{})) |> toEqual(false))

  test("String.contains false", () =>
    expect(List.String.contains("a", list{"b", "c"})) |> toEqual(false)
  )

  test("String.contains true", () =>
    expect(List.String.contains("a", list{"b", "c", "a"})) |> toEqual(true)
  )

  test("String.indexOf missing", () =>
    expect(List.String.indexOf("x", list{"a", "b", "c", "d", "e"})) |> toEqual(None)
  )

  test("String.indexOf found", () =>
    expect(List.String.indexOf("d", list{"a", "b", "c", "d", "e"})) |> toEqual(Some(3))
  )

  test("String.distinct", () =>
    expect(List.String.distinct(list{"foo", "bar", "baz", "bar"})) |> toEqual(list{
      "foo",
      "bar",
      "baz",
    })
  )

  test("String.removeFirst", () =>
    expect(List.String.removeFirst("b", list{"b", "a", "b", "c"})) |> toEqual(list{"a", "b", "c"})
  )

  test("String.removeEach", () =>
    expect(List.String.removeEach("b", list{"b", "a", "b", "c"})) |> toEqual(list{"a", "c"})
  )

  test("String.sort", () =>
    expect(List.String.sort(list{"c", "d", "a", "b", "c"})) |> toEqual(list{
      "a",
      "b",
      "c",
      "c",
      "d",
    })
  )

  test("String.join empty", () => expect(List.String.join(list{})) |> toEqual(""))

  test("String.join one", () => expect(List.String.join(list{"foo"})) |> toEqual("foo"))

  test("String.join many", () => expect(List.String.join(list{"foo", "bar"})) |> toEqual("foobar"))

  test("String.joinWith empty", () => expect(List.String.joinWith(", ", list{})) |> toEqual(""))

  test("String.joinWith", () =>
    expect(List.String.joinWith(", ", list{"a", "b", "c"})) |> toEqual("a, b, c")
  )

  test("Int.sum empty", () => expect(List.Int.sum(list{})) |> toEqual(0))

  test("Int.sum one", () => expect(List.Int.sum(list{1})) |> toEqual(1))

  test("Int.sum many", () => expect(List.Int.sum(list{1, 3, 5})) |> toEqual(9))

  test("Option.traverse success", () =>
    expect(list{1, 2, 3, 4} |> List.Option.traverse(a => Some(a))) |> toEqual(
      Some(list{1, 2, 3, 4}),
    )
  )

  test("Option.traverse failure", () =>
    expect(
      list{1, 2, 3, 4} |> List.Option.traverse(a => mod(a, 2) == 0 ? Some(a) : None),
    ) |> toEqual(None)
  )

  test("Validation.traverse success", () =>
    expect(List.Validation.traverse(a => Ok(a), list{1, 2, 3, 4, 5})) |> toEqual(
      Relude_Validation.VOk(list{1, 2, 3, 4, 5}),
    )
  )

  test("Validation.traverse failure", () =>
    expect(
      List.Validation.traverse(a => Error(string_of_int(a) ++ " is bad"), list{1, 2, 3, 4, 5}),
    ) |> toEqual(
      Relude_Validation.VError(
        Relude_NonEmpty.List.make("1 is bad", list{"2 is bad", "3 is bad", "4 is bad", "5 is bad"}),
      ),
    )
  )

  testAsync("List.IO.sequence", onDone => {
    // Try a bunch of random IOs to seek out problems
    let io1 = IO.pure(1)

    let io2 = IO.suspend(() => 2)

    let io3 = IO.suspendIO(() => IO.pure(3))

    let io4 = IO.async(onDone => Js.Global.setTimeout(() => onDone(Ok(4)), 0) |> ignore)
    let io5 = io4 |> IO.map(four => four + 1)

    let io6 = io4 |> IO.flatMap(four => IO.pure(four + 2))

    let io7 =
      io4
      |> IO.flatMap(four => IO.suspend(() => four + 2))
      |> IO.flatMap(six => IO.async(onDone => onDone(Ok(six + 1))))

    let io8 =
      io7 |> IO.flatMap(seven => Relude.Js.Promise.toIOLazy(() => Js.Promise.resolve(seven + 1)))

    let io9 = IO.throw(9) |> IO.flip

    let io10 =
      io7
      |> IO.summonError
      |> IO.unsummonError
      |> IO.flip
      |> IO.flip
      |> IO.withDelay(0)
      |> IO.flatMap(seven => IO.suspend(() => seven + 3))

    let ios = list{io1, io2, io3, io4, io5, io6, io7, io8, io9, io10}

    List.IO.sequence(ios) |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) => onDone(pass)
      | _ => onDone(fail("fail"))
      }
    )
  })
})
