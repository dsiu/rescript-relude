open Jest
open Expect

module IntSet = Relude_Set.WithOrd(Relude_Int.Ord)

describe("Set", () => {
  test("length of empty set", () =>
    expect({
      open IntSet
      empty->length
    })->toEqual(0)
  )

  test("length of non-empty set", () =>
    expect({
      open IntSet
      singleton(1)->length
    })->toEqual(1)
  )

  test("isEmpty (empty set)", () =>
    expect({
      open IntSet
      empty->isEmpty
    })->toEqual(true)
  )

  test("isEmpty (nonempty set)", () =>
    expect({
      open IntSet
      singleton(1)->isEmpty
    })->toEqual(false)
  )

  test("fromArray (empty)", () => expect(IntSet.fromArray([]))->toEqual(IntSet.empty))

  test("fromArray (nonEmpty, unique)", () =>
    expect({
      open IntSet
      fromArray([1, 2, 3])->length
    })->toEqual(3)
  )

  test("fromArray (nonEmpty, removes duplicates)", () =>
    expect({
      open IntSet
      fromArray([1, 2, 1])->length
    })->toEqual(2)
  )

  test("fromList (empty)", () => expect(IntSet.fromList(list{}))->toEqual(IntSet.empty))

  test("fromList (nonEmpty)", () =>
    expect({
      open IntSet
      fromList(list{4, 4, 4, 4, 3})->length
    })->toEqual(2)
  )

  test("contains (false)", () =>
    expect({
      open IntSet
      fromList(list{1, 3, 3, 4})->(contains(5, _))
    })->toEqual(false)
  )

  test("contains (true)", () =>
    expect({
      open IntSet
      fromList(list{1, 3, 3, 4})->(contains(3, _))
    })->toEqual(true)
  )

  test("add (new unique value)", () =>
    expect({
      open IntSet
      fromList(list{1, 2})->(add(3, _))
    })->toEqual(IntSet.fromList(list{1, 2, 3}))
  )

  test("add (duplicate value)", () =>
    expect({
      open IntSet
      fromList(list{1, 2})->(add(1, _))
    })->toEqual(IntSet.fromList(list{1, 2}))
  )

  test("mergeMany (empty array to merge)", () =>
    expect({
      open IntSet
      singleton(1)->(mergeMany([], _))
    })->toEqual(IntSet.fromList(list{1}))
  )

  test("mergeMany (all unique)", () =>
    expect({
      open IntSet
      singleton(1)->mergeMany([2, 3], _)->eq(fromList(list{1, 2, 3}))
    })->toEqual(true)
  )

  test("mergeMany (some duplicates)", () =>
    expect({
      open IntSet
      fromList(list{1, 2, 3})->mergeMany([2, 3, 4], _)->eq(fromList(list{1, 2, 3, 4}))
    })->toEqual(true)
  )
})
