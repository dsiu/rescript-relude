@@uncurried
@@uncurried.swap

open Jest
open Expect
open! Relude.Globals
open Relude.TreeZipper

let testTree1 = Tree.make(
  1,
  list{
    Tree.make(
      2,
      list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
    ),
    Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
    Tree.make(4, list{}),
  },
)

let testTree2 = Tree.make(
  1,
  list{
    Tree.make(
      2,
      list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212), Tree.pure(213)}),
        Tree.pure(22),
        Tree.pure(23),
        Tree.make(
          24,
          list{
            Tree.pure(241),
            Tree.pure(242),
            Tree.make(243, list{Tree.pure(2431), Tree.pure(2432), Tree.pure(2433)}),
          },
        ),
      },
    ),
    Tree.pure(3),
    Tree.pure(4),
    Tree.make(
      5,
      list{
        Tree.pure(51),
        Tree.pure(52),
        Tree.make(53, list{Tree.pure(531), Tree.pure(532), Tree.pure(533)}),
      },
    ),
  },
)

describe("TreeZipper", () => {
  test("pure", () =>
    expect(TreeZipper.pure(42))->toEqual({
      ancestors: list{},
      leftSiblings: list{},
      focus: 42,
      rightSiblings: list{},
      children: list{},
    })
  )

  test("make", () =>
    expect(
      TreeZipper.make(
        list{(list{Tree.pure(1)}, 2, list{Tree.pure(3)})},
        list{Tree.pure(4)},
        5,
        list{Tree.pure(6)},
        list{Tree.pure(7)},
      ),
    )->toEqual({
      ancestors: list{(list{Tree.pure(1)}, 2, list{Tree.pure(3)})},
      leftSiblings: list{Tree.pure(4)},
      focus: 5,
      rightSiblings: list{Tree.pure(6)},
      children: list{Tree.pure(7)},
    })
  )

  test("makeWithLabels", () =>
    expect(
      TreeZipper.makeWithLabels(
        ~ancestors=list{(list{Tree.pure(1)}, 2, list{Tree.pure(3)})},
        ~leftSiblings=list{Tree.pure(4)},
        ~focus=5,
        ~rightSiblings=list{Tree.pure(6)},
        ~children=list{Tree.pure(7)},
      ),
    )->toEqual({
      ancestors: list{(list{Tree.pure(1)}, 2, list{Tree.pure(3)})},
      leftSiblings: list{Tree.pure(4)},
      focus: 5,
      rightSiblings: list{Tree.pure(6)},
      children: list{Tree.pure(7)},
    })
  )

  test("fromTree", () => {
    let actual = TreeZipper.fromTree(testTree1)
    let expected = {
      ancestors: list{},
      leftSiblings: list{},
      focus: testTree1->Tree.getValue,
      rightSiblings: list{},
      children: testTree1->Tree.getChildren,
    }
    expect(actual)->toEqual(expected)
  })

  test("getAncestors", () => {
    let actual =
      testTree1->TreeZipper.fromTree->moveDownTimes(3, _)->(Option.map(TreeZipper.getAncestors, _))
    let expected = Some(list{
      (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
      (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
      (list{}, 1, list{}),
    })
    expect(actual)->toEqual(expected)
  })

  test("getFocusValue", () => {
    let actual =
      testTree1->TreeZipper.fromTree->moveDownTimes(3, _)->(Option.map(TreeZipper.getFocusValue, _))
    let expected = Some(211)
    expect(actual)->toEqual(expected)
  })

  test("tapFocusValue", () => {
    let r = ref(None)
    testTree1->TreeZipper.fromTree->TreeZipper.tapFocusValue(a => r := Some(a), _)->ignore
    let actual = r.contents
    let expected = Some(1)
    expect(actual)->toEqual(expected)
  })

  test("setFocusValue", () => {
    let actual =
      testTree1->TreeZipper.fromTree->moveDownTimes(3, _)->(Option.map(setFocusValue(42, _), _))
    let expected = Some({
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 42,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("modifyFocusValue", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->moveDownTimes(3, _)
      ->(Option.map(modifyFocusValue(a => a + 42, _), _))
    let expected = Some({
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 211 + 42,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("getFocusTree", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.map(TreeZipper.getFocusTree, _))
    let expected: option<Tree.t<int>> = Some({
      value: 3,
      children: list{Tree.make(31, list{Tree.pure(311)})},
    })
    expect(actual)->toEqual(expected)
  })

  test("getLeftSiblings", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRightTimes(2, _), _)
      ->(Option.map(TreeZipper.getLeftSiblings, _))
    let expected = Some(list{
      Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
      Tree.make(
        2,
        list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
      ),
    })
    expect(actual)->toEqual(expected)
  })

  test("getLeftSiblingsInOrder", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRightTimes(2, _), _)
      ->(Option.map(TreeZipper.getLeftSiblingsInOrder, _))
    let expected = Some(list{
      Tree.make(
        2,
        list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
      ),
      Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
    })
    expect(actual)->toEqual(expected)
  })

  test("setLeftSiblings", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.flatMap(TreeZipper.setLeftSiblings(list{Tree.pure(42), Tree.pure(43)}, _), _))

    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{Tree.pure(42), Tree.pure(43)},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })

    expect(actual)->toEqual(expected)
  })

  test("setLeftSiblingsFromInOrder", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(
        Option.flatMap(
          TreeZipper.setLeftSiblingsFromInOrder(list{Tree.pure(43), Tree.pure(42)}, _),
          _,
        )
      )

    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{Tree.pure(42), Tree.pure(43)},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })

    expect(actual)->toEqual(expected)
  })

  test("getRightSiblings", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.map(TreeZipper.getRightSiblings, _))
    let expected = Some(list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)})
    expect(actual)->toEqual(expected)
  })

  test("setRightSiblings", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.flatMap(TreeZipper.setRightSiblings(list{Tree.pure(42), Tree.pure(43)}, _), _))

    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{Tree.pure(42), Tree.pure(43)},
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })

    expect(actual)->toEqual(expected)
  })

  test("getChildren", () => {
    let actual =
      testTree1->TreeZipper.fromTree->TreeZipper.moveDown->(Option.map(TreeZipper.getChildren, _))
    let expected = Some(list{
      Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
      Tree.pure(22),
      Tree.pure(23),
    })
    expect(actual)->toEqual(expected)
  })

  test("setChildren", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->(TreeZipper.setChildren(list{Tree.pure(42), Tree.pure(43)}, _))
    let expected = {
      ancestors: list{},
      leftSiblings: list{},
      focus: 1,
      rightSiblings: list{},
      children: list{Tree.pure(42), Tree.pure(43)},
    }
    expect(actual)->toEqual(expected)
  })

  test("moveLeft None", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveLeft
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveLeft", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRight, _)
      ->(Option.flatMap(TreeZipper.moveLeft, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("moveLeftWithClamp", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveLeftWithClamp
    let expected = testTree1->TreeZipper.fromTree
    expect(actual)->toEqual(expected)
  })

  test("moveLeftToStart", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRightTimes(2, _), _)
      ->(Option.map(TreeZipper.moveLeftToStart, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("moveLeftTimes", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRightTimes(2, _), _)
      ->(Option.flatMap(TreeZipper.moveLeftTimes(2, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("moveLeftTimes negative", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveLeftTimes(-1, _))
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveLeftTimesWithClamp", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRightTimes(2, _), _)
      ->(Option.map(TreeZipper.moveLeftTimesWithClamp(5, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("moveRight None", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveRight
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveRight", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.flatMap(TreeZipper.moveRight, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 3,
      rightSiblings: list{Tree.make(4, list{})},
      children: list{Tree.make(31, list{Tree.pure(311)})},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveRightWithClamp", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveRightWithClamp
    let expected = testTree1->TreeZipper.fromTree
    expect(actual)->toEqual(expected)
  })

  test("moveRightTimes", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.flatMap(TreeZipper.moveRightTimes(2, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 4,
      rightSiblings: list{},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveRightTimes negative", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveRightTimes(-1, _))
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveRightTimesWithClamp", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.map(TreeZipper.moveRightTimesWithClamp(4, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 4,
      rightSiblings: list{},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveRightToEnd", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->(Option.map(TreeZipper.moveRightToEnd, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 4,
      rightSiblings: list{},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveUp None", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveUp
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveUpWithClamp", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveUpWithClamp
    let expected = testTree1->TreeZipper.fromTree
    expect(actual)->toEqual(expected)
  })

  test("moveUpTimes", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDownTimes(2, _)
      ->(Option.flatMap(TreeZipper.moveUpTimes(2, _), _))
    let expected = Some(testTree1->TreeZipper.fromTree)
    expect(actual)->toEqual(expected)
  })

  test("moveUpTimes negative", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveUpTimes(-1, _))
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveUpTimesWithClamp", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDownTimes(2, _)
      ->(Option.map(TreeZipper.moveUpTimesWithClamp(5, _), _))
    let expected = Some(testTree1->TreeZipper.fromTree)
    expect(actual)->toEqual(expected)
  })

  test("moveUpToTop", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDownTimes(2, _)
      ->(Option.map(TreeZipper.moveUpToTop, _))
    let expected = Some(testTree1->TreeZipper.fromTree)
    expect(actual)->toEqual(expected)
  })

  test("moveUpToTop maintains structure", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveBy(list{#Down(2), #Right(2)}, _)
      ->(Option.map(TreeZipper.moveUpToTop, _))
    let expected = Some(testTree1->TreeZipper.fromTree)
    expect(actual)->toEqual(expected)
  })

  test("moveDown", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveDown
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("moveDownWithClamp", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDownTimes(3, _)
      ->(Option.map(TreeZipper.moveDownWithClamp, _))
    let expected = Some({
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 211,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveDownToBottom", () => {
    let actual = testTree1->TreeZipper.fromTree->TreeZipper.moveDownToBottom
    let expected = {
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 211,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    }
    expect(actual)->toEqual(expected)
  })

  test("moveDownTimes", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveDownTimes(3, _))
    let expected = Some({
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 211,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("moveDownTimes negative", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveDownTimes(-1, _))
    let expected = None
    expect(actual)->toEqual(expected)
  })

  test("moveDownTimesWithClamp", () => {
    let actual = testTree1->TreeZipper.fromTree->(TreeZipper.moveDownTimesWithClamp(5, _))
    let expected = {
      ancestors: list{
        (list{}, 21, list{Tree.pure(22), Tree.pure(23)}),
        (list{}, 2, list{Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}), Tree.pure(4)}),
        (list{}, 1, list{}),
      },
      leftSiblings: list{},
      focus: 211,
      rightSiblings: list{Tree.pure(212)},
      children: list{},
    }
    expect(actual)->toEqual(expected)
  })

  test("moveBy", () => {
    let actual =
      testTree1
      ->fromTree
      ->(
        moveBy(
          list{
            #Down(1),
            #Right(1),
            #Left(1),
            #Up(1),
            #DownWithClamp(1),
            #RightWithClamp(1),
            #LeftWithClamp(1),
            #UpWithClamp(1),
            #DownToBottom,
            #UpToTop,
            #RightToEnd,
            #LeftToStart,
            #Down(1),
            #Right(1),
            #Down(1),
          },
          _,
        )
      )

    let expected =
      testTree1->fromTree->moveDown->Option.flatMap(moveRight, _)->(Option.flatMap(moveDown, _))
    expect(actual)->toEqual(expected)
  })

  test("foldBy", () => {
    let actual =
      testTree1
      ->fromTree
      ->(
        foldBy(
          list{
            #Down(1),
            #Right(1),
            #Left(1),
            #Up(1),
            #DownWithClamp(1),
            #RightWithClamp(1),
            #LeftWithClamp(1),
            #UpWithClamp(1),
            #DownToBottom,
            #UpToTop,
            #RightToEnd,
            #LeftToStart,
            #Down(1),
            #Right(1),
            #Down(1),
          },
          (l, v) => l->(Relude_List.append(v, _)),
          list{},
          _,
        )
      )
    let expectedZipper =
      testTree1->fromTree->moveDown->Option.flatMap(moveRight, _)->(Option.flatMap(moveDown, _))
    expect(actual)->toEqual(
      expectedZipper->(
        Option.map(z => (z, list{2, 3, 2, 1, 2, 3, 2, 1, 211, 1, 1, 1, 2, 3, 31}), _)
      ),
    )
  })

  test("map", () => {
    let actual =
      testTree1
      ->TreeZipper.fromTree
      ->TreeZipper.moveDown
      ->Option.flatMap(TreeZipper.moveRight, _)
      ->(Option.map(TreeZipper.map(string_of_int, _), _))
    let expected = Some({
      ancestors: list{(list{}, "1", list{})},
      leftSiblings: list{
        Tree.make(
          "2",
          list{
            Tree.make("21", list{Tree.pure("211"), Tree.pure("212")}),
            Tree.pure("22"),
            Tree.pure("23"),
          },
        ),
      },
      focus: "3",
      rightSiblings: list{Tree.make("4", list{})},
      children: list{Tree.make("31", list{Tree.pure("311")})},
    })
    expect(actual)->toEqual(expected)
  })

  test("findInFocus", () => {
    let actual = testTree1->fromTree->(findInFocus(a => a == 1, _))
    let expected = testTree1->fromTree->Relude_Option.pure // 1
    let actual2 = testTree1->fromTree->(findInFocus(a => a == 0, _))
    let expected2 = None
    expect((actual, actual2))->toEqual((expected, expected2))
  })

  test("findInFocusAndChildren", () => {
    let a = testTree2->fromTree->(findInFocusAndChildren(a => a == 213, _))
    let e = testTree2->fromTree->moveDownTimes(3, _)->(Option.flatMap(moveRightTimes(2, _), _)) // 1 // 211 // 213
    let a2 = testTree2->fromTree->(findInFocusAndChildren(a => a == 2433, _))
    // 1 -> 21 -> 24 -> 241 -> 243 -> 2431 -> 2433
    let e2 =
      testTree2
      ->fromTree
      ->moveDownTimes(2, _)
      ->Option.flatMap(moveRightTimes(3, _), _)
      ->Option.flatMap(moveDown, _)
      ->Option.flatMap(moveRightTimes(2, _), _)
      ->Option.flatMap(moveDown, _)
      ->(Option.flatMap(moveRightTimes(2, _), _))
    let a3 = testTree2->fromTree->(findInFocusAndChildren(a => a == 533, _))
    // 1 -> 2 -> 5 -> 51 -> 53 -> 531 -> 533
    let e3 =
      testTree2
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRightTimes(3, _), _)
      ->Option.flatMap(moveDown, _)
      ->Option.flatMap(moveRightTimes(2, _), _)
      ->Option.flatMap(moveDown, _)
      ->(Option.flatMap(moveRightTimes(2, _), _))
    expect((a, a2, a3))->toEqual((e, e2, e3))
  })

  test("findLeft", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(findLeft(a => a == 2, _), _))
    let expected = testTree1->fromTree->moveDown
    expect(actual)->toEqual(expected)
  })

  test("findRight", () => {
    let actual = testTree1->fromTree->moveDown->(Option.flatMap(findRight(a => a == 4, _), _))
    let expected = testTree1->fromTree->moveDown->(Option.flatMap(moveRightTimes(2, _), _))
    expect(actual)->toEqual(expected)
  })

  test("findUp", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDownTimes(3, _)
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(findUp(a => a == 1, _), _))
    let expected = Some(testTree1->fromTree)
    expect(actual)->toEqual(expected)
  })

  test("findDown", () => {
    let actual = testTree1->fromTree->(findDown(a => a == 311, _))
    // 1 -> 2 -> 3 -> 31, 311
    let expected =
      testTree1
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(moveDownTimes(2, _), _))
    expect(actual)->toEqual(expected)
  })

  test("find", () => {
    let actual = testTree1->fromTree->moveDown->(Option.flatMap(find(a => a == 311, _), _))
    // 1-> 2 -> 3 -> 31, 311
    let expected =
      testTree1
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(moveDownTimes(2, _), _))
    expect(actual)->toEqual(expected)
  })

  test("insertTreeWithPushLeft", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDown
      ->(Option.flatMap(insertTreeWithPushLeft(Tree.make(42, list{Tree.pure(43)}), _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 42,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{Tree.pure(43)},
    })
    expect(actual)->toEqual(expected)
  })

  test("insertWithPushLeft", () => {
    let actual = testTree1->fromTree->moveDown->(Option.flatMap(insertWithPushLeft(42, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 42,
      rightSiblings: list{
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("insertTreeWithPushRight", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDown
      ->(Option.flatMap(insertTreeWithPushRight(Tree.make(42, list{Tree.pure(43)}), _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 42,
      rightSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{Tree.pure(43)},
    })
    expect(actual)->toEqual(expected)
  })

  test("insertWithPushRight", () => {
    let actual = testTree1->fromTree->moveDown->(Option.flatMap(insertWithPushRight(42, _), _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 42,
      rightSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
        Tree.make(3, list{Tree.make(31, list{Tree.pure(311)})}),
        Tree.make(4, list{}),
      },
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("deleteWithPullLeft", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(deleteWithPullLeft, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 2,
      rightSiblings: list{Tree.make(4, list{})},
      children: list{
        Tree.make(21, list{Tree.pure(211), Tree.pure(212)}),
        Tree.pure(22),
        Tree.pure(23),
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("deleteWithPullRight", () => {
    let actual =
      testTree1
      ->fromTree
      ->moveDown
      ->Option.flatMap(moveRight, _)
      ->(Option.flatMap(deleteWithPullRight, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{
        Tree.make(
          2,
          list{Tree.make(21, list{Tree.pure(211), Tree.pure(212)}), Tree.pure(22), Tree.pure(23)},
        ),
      },
      focus: 4,
      rightSiblings: list{},
      children: list{},
    })
    expect(actual)->toEqual(expected)
  })

  test("delete", () => {
    let actual = testTree1->fromTree->moveDown->(Option.flatMap(delete, _))
    let expected = Some({
      ancestors: list{(list{}, 1, list{})},
      leftSiblings: list{},
      focus: 3,
      rightSiblings: list{Tree.pure(4)},
      children: list{Tree.make(31, list{Tree.pure(311)})},
    })
    expect(actual)->toEqual(expected)
  })
})
