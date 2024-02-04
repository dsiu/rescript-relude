@@uncurried
@@uncurried.swap

open Jest
open Expect

module S = Relude.Sequence
module LS = S.List
module AS = S.Array
module LZ = Relude.ListZipper
type t<'a> = LZ.t<'a> = Zipper(list<'a>, 'a, list<'a>)

open LZ.Infix

module LZFoldMap = LZ.Foldable.Fold_Map(Relude.String.Monoid)
module LZFoldMapAny = LZ.Foldable.Fold_Map_Any(Relude.List.MonoidAny)
module LZTraversable = LZ.Traversable(Relude.Option.Applicative)

describe("ListZipper", () => {
  test("make", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5}))->toEqual(Zipper(list{2, 1}, 3, list{4, 5}))
  )

  test("makeWithLeft", () =>
    expect(LZ.makeWithLeft(list{2, 1}, 3))->toEqual(Zipper(list{2, 1}, 3, list{}))
  )

  test("makeWithRight", () =>
    expect(LZ.makeWithRight(3, list{4, 5}))->toEqual(Zipper(list{}, 3, list{4, 5}))
  )

  test("makeWithRightArray", () =>
    expect(LZ.makeWithRightArray(3, [4, 5]))->toEqual(Zipper(list{}, 3, list{4, 5}))
  )

  test("makeWithRightList", () =>
    expect(LZ.makeWithRightList(3, list{4, 5}))->toEqual(Zipper(list{}, 3, list{4, 5}))
  )

  test("makeWithFocus", () => expect(LZ.makeWithFocus(3))->toEqual(Zipper(list{}, 3, list{})))

  test("fromSequence list non-empty", () =>
    expect(LZ.fromSequence(LS.fromList(list{1, 2, 3, 4, 5})))->toEqual(
      Some(Zipper(list{}, 1, list{2, 3, 4, 5})),
    )
  )

  test("fromSequence list empty", () => expect(LZ.fromSequence(LS.fromList(list{})))->toEqual(None))

  test("fromSequence array non-empty", () =>
    expect(LZ.fromSequence(LS.fromArray([1, 2, 3, 4, 5])))->toEqual(
      Some(Zipper(list{}, 1, list{2, 3, 4, 5})),
    )
  )

  test("fromSequence array empty", () => expect(LZ.fromSequence(LS.fromArray([])))->toEqual(None))

  test("fromArray non-empty", () =>
    expect(LZ.fromArray([1, 2, 3, 4, 5]))->toEqual(Some(Zipper(list{}, 1, list{2, 3, 4, 5})))
  )

  test("fromArray empty", () => expect(LZ.fromArray([]))->toEqual(None))

  test("fromList non-empty", () =>
    expect(LZ.fromList(list{1, 2, 3, 4, 5}))->toEqual(Some(Zipper(list{}, 1, list{2, 3, 4, 5})))
  )

  test("fromList empty", () => expect(LZ.fromList(list{}))->toEqual(None))

  test("fromNonEmptyArray", () =>
    expect(LZ.fromNea(Relude.Nea.make(1, [2, 3, 4, 5])))->toEqual(
      Zipper(list{}, 1, list{2, 3, 4, 5}),
    )
  )

  test("fromNonEmptyList", () =>
    expect(LZ.fromNel(Relude.Nel.make(1, list{2, 3, 4, 5})))->toEqual(
      Zipper(list{}, 1, list{2, 3, 4, 5}),
    )
  )

  test("map", () =>
    expect(\"<$$>"(LZ.make(list{2, 1}, 3, list{4, 5}), a => a * 10))->toEqual(
      LZ.make(list{20, 10}, 30, list{40, 50}),
    )
  )

  test("apply", () => {
    let l = list{a => a * 10, a => a * 100}
    let f = a => a * 1000
    let r = list{a => a * 10000, a => a * 100000}
    let actual = \"<*>"(LZ.make(l, f, r), LZ.make(list{2, 1}, 3, list{4, 5}))
    let expected = LZ.make(list{20, 100}, 3000, list{40000, 500000})
    expect(actual)->toEqual(expected)
  })

  test("pure", () => expect(LZ.pure(42))->toEqual(LZ.makeWithFocus(42)))

  test("foldLeft", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(
        LZ.foldLeft((acc, a) => acc->(Relude.List.append(a, _)), list{}, _)
      ),
    )->toEqual(list{1, 2, 3, 4, 5})
  )

  test("foldRight", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(
        LZ.foldRight((a, acc) => acc->(Relude.List.append(a, _)), list{}, _)
      ),
    )->toEqual(list{5, 4, 3, 2, 1})
  )

  test("foldMap", () =>
    expect(LZFoldMap.fold_map(string_of_int, LZ.make(list{2, 1}, 3, list{4, 5})))->toEqual("12345")
  )

  test("foldMapAny", () =>
    expect(LZFoldMapAny.fold_map(a => list{a}, LZ.make(list{2, 1}, 3, list{4, 5})))->toEqual(list{
      1,
      2,
      3,
      4,
      5,
    })
  )

  test("traverse", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZTraversable.traverse(a => Some(string_of_int(a)), _)),
    )->toEqual(Some(LZ.make(list{"2", "1"}, "3", list{"4", "5"})))
  )

  test("sequence", () =>
    expect(
      LZ.make(list{Some(2), Some(1)}, Some(3), list{Some(4), Some(5)})->LZTraversable.sequence,
    )->toEqual(Some(LZ.make(list{2, 1}, 3, list{4, 5})))
  )

  test("toSequence", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.toSequence)->toEqual(
      LS.fromList(list{1, 2, 3, 4, 5}),
    )
  )

  test("toArray", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.toArray)->toEqual([1, 2, 3, 4, 5])
  )

  test("toList", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.toList)->toEqual(list{1, 2, 3, 4, 5})
  )

  test("toNonEmptyArray", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.toNea)->toEqual(Relude.Nea.make(1, [2, 3, 4, 5]))
  )

  test("toNonEmptyArray empty left", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.toNea)->toEqual(Relude.Nea.make(3, [4, 5]))
  )

  test("toNonEmptyList", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.toNel)->toEqual(
      Relude.Nel.make(1, list{2, 3, 4, 5}),
    )
  )

  test("toNonEmptyList empty left", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.toNel)->toEqual(Relude.Nel.make(3, list{4, 5}))
  )

  test("concatWithKeepLeftFocus", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{20, 10}, 30, list{40, 50})
    let z = LZ.concatWithKeepLeftFocus(~prefix=x, y)
    expect((z, z->LZ.toList))->toEqual((
      LZ.make(list{2, 1}, 3, list{4, 5, 10, 20, 30, 40, 50}),
      list{1, 2, 3, 4, 5, 10, 20, 30, 40, 50},
    ))
  })

  test("concatWithKeepRightFocus", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{20, 10}, 30, list{40, 50})
    let z = LZ.concatWithKeepRightFocus(~prefix=x, y)
    expect((z, z->LZ.toList))->toEqual((
      LZ.make(list{20, 10, 5, 4, 3, 2, 1}, 30, list{40, 50}),
      list{1, 2, 3, 4, 5, 10, 20, 30, 40, 50},
    ))
  })

  test("reverse", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.reverse)->toEqual(
      LZ.make(list{4, 5}, 3, list{2, 1}),
    )
  )

  test("zipWith", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{20, 10}, 30, list{40, 50})
    let z = LZ.zipWith((a, b) => a + b, x, y)
    expect(z)->toEqual(LZ.make(list{22, 11}, 33, list{44, 55}))
  })

  test("zip", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{20, 10}, 30, list{40, 50})
    let z = LZ.zip(x, y)
    expect(z)->toEqual(LZ.make(list{(2, 20), (1, 10)}, (3, 30), list{(4, 40), (5, 50)}))
  })

  test("zipWithIndex", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.zipWithIndex)->toEqual(
      LZ.make(list{(2, 1), (1, 0)}, (3, 2), list{(4, 3), (5, 4)}),
    )
  )

  test("getFocus", () => expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.getFocus)->toEqual(3))

  test("setFocusBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.setFocusBy(a => a * 10, _)))->toEqual(
      LZ.make(list{2, 1}, 30, list{4, 5}),
    )
  )

  test("setFocus", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.setFocus(42, _)))->toEqual(
      LZ.make(list{2, 1}, 42, list{4, 5}),
    )
  )

  test("getLeft", () => expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.getLeft)->toEqual(list{2, 1}))

  test("setLeft", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.setLeft(list{42, 55}, _)))->toEqual(
      LZ.make(list{42, 55}, 3, list{4, 5}),
    )
  )

  test("getRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.getRight)->toEqual(list{4, 5})
  )

  test("setRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.setRight(list{42, 55}, _)))->toEqual(
      LZ.make(list{2, 1}, 3, list{42, 55}),
    )
  )

  test("getLeftInOrder", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.getLeftInOrder)->toEqual(list{1, 2})
  )

  test("setLeftInOrder", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.setLeftFromInOrder(list{42, 55}, _)))->toEqual(
      LZ.make(list{55, 42}, 3, list{4, 5}),
    )
  )

  test("peekLeft", () => expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.peekLeft)->toEqual(Some(2)))

  test("peekRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.peekRight)->toEqual(Some(4))
  )

  test("isAtStart not at start", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.isAtStart)->toEqual(false)
  )

  test("isAtStart is at start", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.isAtStart)->toEqual(true)
  )

  test("isAtEnd not at end", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.isAtEnd)->toEqual(false)
  )

  test("isAtEnd is at end", () => expect(LZ.make(list{2, 1}, 3, list{})->LZ.isAtEnd)->toEqual(true))

  test("isAtIndex true", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->(LZ.isAtIndex(2, _)))->toEqual(true)
  )

  test("isAtIndex false", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->(LZ.isAtIndex(1, _)))->toEqual(false)
  )

  test("isAtItemBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->(LZ.isAtItemBy(Relude_Int.Eq.eq, 3, _)))->toEqual(true)
  )

  test("isAtItem", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->(LZ.isAtItem(module(Relude_Int.Eq), 3, _)))->toEqual(
      true,
    )
  )

  test("moveLeft", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveLeft)->toEqual(
      Some(LZ.make(list{1}, 2, list{3, 4, 5})),
    )
  )

  test("moveLeft at start", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.moveLeft)->toEqual(None)
  )

  test("moveRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveRight)->toEqual(
      Some(LZ.make(list{3, 2, 1}, 4, list{5})),
    )
  )

  test("moveRight at end", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->LZ.moveRight)->toEqual(None)
  )

  test("moveLeftWithClamp", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveLeftWithClamp)->toEqual(
      LZ.make(list{1}, 2, list{3, 4, 5}),
    )
  )

  test("moveLeftWithClamp at start", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.moveLeftWithClamp)->toEqual(
      LZ.make(list{}, 3, list{4, 5}),
    )
  )

  test("moveRightWithClamp", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveRightWithClamp)->toEqual(
      LZ.make(list{3, 2, 1}, 4, list{5}),
    )
  )

  test("moveRightWithClamp at end", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->LZ.moveRightWithClamp)->toEqual(
      LZ.make(list{2, 1}, 3, list{}),
    )
  )

  test("moveStart", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveStart)->toEqual(
      LZ.make(list{}, 1, list{2, 3, 4, 5}),
    )
  )

  test("moveStart at start", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.moveStart)->toEqual(LZ.make(list{}, 3, list{4, 5}))
  )

  test("moveEnd", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveEnd)->toEqual(
      LZ.make(list{4, 3, 2, 1}, 5, list{}),
    )
  )

  test("moveEnd at end", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->LZ.moveEnd)->toEqual(LZ.make(list{2, 1}, 3, list{}))
  )

  test("moveLeftWithWrap", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveLeftWithWrap)->toEqual(
      LZ.make(list{1}, 2, list{3, 4, 5}),
    )
  )

  test("moveLeftWithWrap at start", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.moveLeftWithWrap)->toEqual(
      LZ.make(list{4, 3}, 5, list{}),
    )
  )

  test("moveRightWithWrap", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.moveRightWithWrap)->toEqual(
      LZ.make(list{3, 2, 1}, 4, list{5}),
    )
  )

  test("moveRightWithWrap at end", () =>
    expect(LZ.make(list{2, 1}, 3, list{})->LZ.moveRightWithWrap)->toEqual(
      LZ.make(list{}, 1, list{2, 3}),
    )
  )

  test("moveLeftTimes", () =>
    expect(LZ.make(list{5, 4, 3, 2, 1}, 6, list{})->(LZ.moveLeftTimes(3, _)))->toEqual(
      Some(LZ.make(list{2, 1}, 3, list{4, 5, 6})),
    )
  )

  test("moveLeftTimes to start", () =>
    expect(LZ.make(list{5, 4, 3, 2, 1}, 6, list{})->(LZ.moveLeftTimes(5, _)))->toEqual(
      Some(LZ.make(list{}, 1, list{2, 3, 4, 5, 6})),
    )
  )

  test("moveLeftTimes past start", () =>
    expect(LZ.make(list{5, 4, 3, 2, 1}, 6, list{})->(LZ.moveLeftTimes(6, _)))->toEqual(None)
  )

  test("moveLeftTimes invalid times", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveLeftTimes(-1, _)))->toEqual(None)
  )

  test("moveRightTimes", () =>
    expect(LZ.make(list{}, 1, list{2, 3, 4, 5, 6})->(LZ.moveRightTimes(3, _)))->toEqual(
      Some(LZ.make(list{3, 2, 1}, 4, list{5, 6})),
    )
  )

  test("moveRightTimes to end", () =>
    expect(LZ.make(list{}, 1, list{2, 3, 4, 5, 6})->(LZ.moveRightTimes(5, _)))->toEqual(
      Some(LZ.make(list{5, 4, 3, 2, 1}, 6, list{})),
    )
  )

  test("moveRightTimes past end", () =>
    expect(LZ.make(list{}, 1, list{2, 3, 4, 5, 6})->(LZ.moveRightTimes(6, _)))->toEqual(None)
  )

  test("moveRightTimes invalid times", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveRightTimes(-1, _)))->toEqual(None)
  )

  test("moveLeftTimesWithClamp", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveLeftTimesWithClamp(5, _)))->toEqual(
      LZ.make(list{}, 1, list{2, 3, 4, 5}),
    )
  )

  test("moveRightTimesWithClamp", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveRightTimesWithClamp(5, _)))->toEqual(
      LZ.make(list{4, 3, 2, 1}, 5, list{}),
    )
  )

  test("moveToIndex no-op", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndex(2, _)))->toEqual(
      Some(LZ.make(list{2, 1}, 3, list{4, 5})),
    )
  )

  test("moveToIndex right", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndex(4, _)))->toEqual(
      Some(LZ.make(list{4, 3, 2, 1}, 5, list{})),
    )
  )

  test("moveToIndex left", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndex(1, _)))->toEqual(
      Some(LZ.make(list{1}, 2, list{3, 4, 5})),
    )
  )

  test("moveToIndex invalid", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndex(5, _)))->toEqual(None)
  )

  test("moveToIndexWithMod no-op", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithMod(2, _)))->toEqual(
      LZ.make(list{2, 1}, 3, list{4, 5}),
    )
  )

  test("moveToIndexWithMod right", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithMod(4, _)))->toEqual(
      LZ.make(list{4, 3, 2, 1}, 5, list{}),
    )
  )

  test("moveToIndexWithMod left", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithMod(1, _)))->toEqual(
      LZ.make(list{1}, 2, list{3, 4, 5}),
    )
  )

  test("moveToIndexWithMod invalid", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithMod(5, _)))->toEqual(
      LZ.make(list{}, 1, list{2, 3, 4, 5}),
    )
  )

  test("moveToIndexWithClamp no-op", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithClamp(2, _)))->toEqual(
      LZ.make(list{2, 1}, 3, list{4, 5}),
    )
  )

  test("moveToIndexWithClamp right", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithClamp(4, _)))->toEqual(
      LZ.make(list{4, 3, 2, 1}, 5, list{}),
    )
  )

  test("moveToIndexWithClamp left", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithClamp(1, _)))->toEqual(
      LZ.make(list{1}, 2, list{3, 4, 5}),
    )
  )

  test("moveToIndexWithClamp invalid", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.moveToIndexWithClamp(5, _)))->toEqual(
      LZ.make(list{4, 3, 2, 1}, 5, list{}),
    )
  )

  test("findLeftBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findLeftBy(a => a == 1, _)))->toEqual(
      Some(LZ.make(list{}, 1, list{2, 3, 4, 5})),
    )
  )

  test("findLeftBy focus", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findLeftBy(a => a == 3, _)))->toEqual(
      Some(LZ.make(list{2, 1}, 3, list{4, 5})),
    )
  )

  test("findLeftBy no focus", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findLeftBy(~checkFocus=false, a => a == 3, _)),
    )->toEqual(None)
  )

  test("findRightBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findRightBy(a => a == 5, _)))->toEqual(
      Some(LZ.make(list{4, 3, 2, 1}, 5, list{})),
    )
  )

  test("findRightBy focus", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findRightBy(a => a == 3, _)))->toEqual(
      Some(LZ.make(list{2, 1}, 3, list{4, 5})),
    )
  )

  test("findRightBy no focus", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findRightBy(~checkFocus=false, a => a == 3, _)),
    )->toEqual(None)
  )

  test("findBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findBy(a => a == 5, _)))->toEqual(
      Some(LZ.make(list{4, 3, 2, 1}, 5, list{})),
    )
  )

  test("findItemLeftBy", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItemLeftBy(Relude_Int.Eq.eq, 1, _)),
    )->toEqual(Some(LZ.make(list{}, 1, list{2, 3, 4, 5})))
  )

  test("findItemRightBy", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItemRightBy(Relude_Int.Eq.eq, 5, _)),
    )->toEqual(Some(LZ.make(list{4, 3, 2, 1}, 5, list{})))
  )

  test("findItemBy", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItemBy(Relude_Int.Eq.eq, 5, _)))->toEqual(
      Some(LZ.make(list{4, 3, 2, 1}, 5, list{})),
    )
  )

  test("findItemLeft", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItemLeft(module(Relude_Int.Eq), 1, _)),
    )->toEqual(Some(LZ.make(list{}, 1, list{2, 3, 4, 5})))
  )

  test("findItemRight", () =>
    expect(
      LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItemRight(module(Relude_Int.Eq), 5, _)),
    )->toEqual(Some(LZ.make(list{4, 3, 2, 1}, 5, list{})))
  )

  test("findItem", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.findItem(module(Relude_Int.Eq), 5, _)))->toEqual(
      Some(LZ.make(list{4, 3, 2, 1}, 5, list{})),
    )
  )

  test("insertWithPushLeft", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.insertWithPushLeft(42, _)))->toEqual(
      LZ.make(list{3, 2, 1}, 42, list{4, 5}),
    )
  )

  test("insertWithPushRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->(LZ.insertWithPushRight(42, _)))->toEqual(
      LZ.make(list{2, 1}, 42, list{3, 4, 5}),
    )
  )

  test("deleteWithPullLeft", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.deleteWithPullLeft)->toEqual(
      Some(LZ.make(list{1}, 2, list{4, 5})),
    )
  )

  test("deleteWithPullRight", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.deleteWithPullRight)->toEqual(
      Some(LZ.make(list{2, 1}, 4, list{5})),
    )
  )

  test("deleteWithPullLeftOrRight left", () =>
    expect(LZ.make(list{2, 1}, 3, list{4, 5})->LZ.deleteWithPullLeftOrRight)->toEqual(
      Some(LZ.make(list{1}, 2, list{4, 5})),
    )
  )

  test("deleteWithPullLeftOrRight right", () =>
    expect(LZ.make(list{}, 3, list{4, 5})->LZ.deleteWithPullLeftOrRight)->toEqual(
      Some(LZ.make(list{}, 4, list{5})),
    )
  )

  test("deleteWithPullLeftOrRight none", () =>
    expect(LZ.make(list{}, 3, list{})->LZ.deleteWithPullLeftOrRight)->toEqual(None)
  )

  test("showBy", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = x->(LZ.showBy(string_of_int, _))
    expect(y)->toEqual("Zipper([2, 1], 3, [4, 5])")
  })

  test("show", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = x->(LZ.show(module(Relude.Int.Show), _))
    expect(y)->toEqual("Zipper([2, 1], 3, [4, 5])")
  })

  test("eqBy true", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{2, 1}, 3, list{4, 5})
    expect(LZ.eqBy(Relude.Int.eq, x, y))->toEqual(true)
  })

  test("eqBy false", () => {
    let x = LZ.make(list{2, 1}, 3, list{4, 5})
    let y = LZ.make(list{2, 1}, 3, list{44, 5})
    expect(LZ.eqBy(Relude.Int.eq, x, y))->toEqual(false)
  })
})
