@@uncurried
@@uncurried.swap

@ocaml.doc("
A zipper for a non-empty multi-way/rose tree

The leftSiblings are stored in reverse order for O(1) sideways movements.

Based on the ideas/implementation from Tony Morris' talk here:
http://data.tmorris.net/talks/zippers/bd054c210649101b84662c614fc45af3c27a5eef/zippers.pdf
")
type t<'a> = {
  ancestors: list<(list<Relude_Tree.t<'a>>, 'a, list<Relude_Tree.t<'a>>)>,
  leftSiblings: list<Relude_Tree.t<'a>>,
  focus: 'a,
  rightSiblings: list<Relude_Tree.t<'a>>,
  children: list<Relude_Tree.t<'a>>,
}

@ocaml.doc("
Creates a tree zipper containing a single item
")
let pure: 'a => t<'a> = a => {
  ancestors: list{},
  leftSiblings: list{},
  focus: a,
  rightSiblings: list{},
  children: list{},
}

@ocaml.doc("
Alias for pure
")
let singleton = pure

@ocaml.doc("
Constructs a tree zipper from parts
")
let make: 'a. (
  list<(list<Relude_Tree.t<'a>>, 'a, list<Relude_Tree.t<'a>>)>,
  list<Relude_Tree.t<'a>>,
  'a,
  list<Relude_Tree.t<'a>>,
  list<Relude_Tree.t<'a>>,
) => t<'a> = (ancestors, leftSiblings, focus, rightSiblings, children) => {
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}

@ocaml.doc("
Constructs a tree zipper from (labelled) parts
")
let makeWithLabels: 'a. (
  ~ancestors: list<(list<Relude_Tree.t<'a>>, 'a, list<Relude_Tree.t<'a>>)>,
  ~leftSiblings: list<Relude_Tree.t<'a>>,
  ~focus: 'a,
  ~rightSiblings: list<Relude_Tree.t<'a>>,
  ~children: list<Relude_Tree.t<'a>>,
) => t<'a> = (~ancestors, ~leftSiblings, ~focus, ~rightSiblings, ~children) => {
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}

@ocaml.doc("
Converts a Tree into a Tree Zipper
")
let fromTree: 'a. Relude_Tree.t<'a> => t<'a> = ({value, children}) =>
  makeWithLabels(
    ~ancestors=list{},
    ~leftSiblings=list{},
    ~focus=value,
    ~rightSiblings=list{},
    ~children,
  )

@ocaml.doc("
Gets the list of ancestor levels for the given TreeZipper
")
let getAncestors: 'a. t<'a> => list<(list<Relude_Tree.t<'a>>, 'a, list<Relude_Tree.t<'a>>)> = ({
  ancestors,
  leftSiblings: _,
  focus: _,
  rightSiblings: _,
  children: _,
}) => ancestors

@ocaml.doc("
Gets the value at the focus of the TreeZipper
")
let getFocusValue: 'a. t<'a> => 'a = ({
  ancestors: _,
  leftSiblings: _,
  focus,
  rightSiblings: _,
  children: _,
}) => focus

@ocaml.doc("
Applies a side-effect function with the current focus value, and returns the zipper unchanged
")
let tapFocusValue: 'a. ('a => unit, t<'a>) => t<'a> = (f, zipper) => {
  f(getFocusValue(zipper))
  zipper
}

@ocaml.doc("
Overwrites the focus with the given value
")
let setFocusValue: 'a. ('a, t<'a>) => t<'a> = (
  newFocus,
  {ancestors, leftSiblings, focus: _, rightSiblings, children},
) => {
  ancestors,
  leftSiblings,
  focus: newFocus,
  rightSiblings,
  children,
}

@ocaml.doc("
Modifies the focus with the given function
")
let modifyFocusValue: 'a. ('a => 'a, t<'a>) => t<'a> = (
  f,
  {ancestors, leftSiblings, focus, rightSiblings, children},
) => {
  ancestors,
  leftSiblings,
  focus: f(focus),
  rightSiblings,
  children,
}

@ocaml.doc("
Gets the value and children at the focus of the TreeZipper as a Tree
")
let getFocusTree: 'a. t<'a> => Relude_Tree.t<'a> = ({
  ancestors: _,
  leftSiblings: _,
  focus,
  rightSiblings: _,
  children,
}) => {
  value: focus,
  children,
}

@ocaml.doc("
Gets the siblings to the left of the focus value, in reverse order.
(the first item of the resulting list is the item that is immediately to the left of the focus).
")
let getLeftSiblings: 'a. t<'a> => list<Relude_Tree.t<'a>> = ({
  ancestors: _,
  leftSiblings,
  focus: _,
  rightSiblings: _,
  children: _,
}) => leftSiblings

@ocaml.doc("
Gets the siblings to the left of the focus value, in order.
(the first item of the resulting list is the item that is the leftmost sibling (furthest from focus))
")
let getLeftSiblingsInOrder: 'a. t<'a> => list<Relude_Tree.t<'a>> = tree =>
  Relude_List.reverse(getLeftSiblings(tree))

@ocaml.doc("
Sets the left siblings from a reversed list (where the first item of the list should be closest to the focus)
")
let setLeftSiblings: 'a. (list<Relude_Tree.t<'a>>, t<'a>) => option<t<'a>> = (
  newLeftSiblings,
  {ancestors, leftSiblings: _, focus, rightSiblings, children},
) =>
  if Relude_List.isNotEmpty(ancestors) {
    Some({
      ancestors,
      leftSiblings: newLeftSiblings,
      focus,
      rightSiblings,
      children,
    })
  } else {
    None
  }

@ocaml.doc("
Sets the left siblings from an in-order list (where the first item of the list should be farthest from the focus)
")
let setLeftSiblingsFromInOrder: 'a. (list<Relude_Tree.t<'a>>, t<'a>) => option<t<'a>> = (
  newLeftSiblingsInOrder,
  {ancestors, leftSiblings: _, focus, rightSiblings, children},
) =>
  if Relude_List.isNotEmpty(ancestors) {
    Some({
      ancestors,
      leftSiblings: Relude_List.reverse(newLeftSiblingsInOrder),
      focus,
      rightSiblings,
      children,
    })
  } else {
    None
  }

@ocaml.doc("
Gets the siblings to the right of the current focus
")
let getRightSiblings: 'a. t<'a> => list<Relude_Tree.t<'a>> = ({
  ancestors: _,
  leftSiblings: _,
  focus: _,
  rightSiblings,
  children: _,
}) => rightSiblings

@ocaml.doc("
Sets the right siblings from a reversed list (where the first item of the list should be closest to the focus)
")
let setRightSiblings: 'a. (list<Relude_Tree.t<'a>>, t<'a>) => option<t<'a>> = (
  newRightSiblings,
  {ancestors, leftSiblings, focus, rightSiblings: _, children},
) =>
  if Relude_List.isNotEmpty(ancestors) {
    Some({
      ancestors,
      leftSiblings,
      focus,
      rightSiblings: newRightSiblings,
      children,
    })
  } else {
    None
  }

@ocaml.doc("
Gets the children sub-trees of the current focus
")
let getChildren: 'a. t<'a> => list<Relude_Tree.t<'a>> = ({
  ancestors: _,
  leftSiblings: _,
  focus: _,
  rightSiblings: _,
  children,
}) => children

@ocaml.doc("
Sets the children
")
let setChildren: 'a. (list<Relude_Tree.t<'a>>, t<'a>) => t<'a> = (
  newChildren,
  {ancestors, leftSiblings, focus, rightSiblings, children: _},
) => {
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children: newChildren,
}

@ocaml.doc("
Moves the focus one sibling to the left (if possible)
")
let moveLeft: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}) => Relude_Option.map(((leftHead, leftTail)) => {
    ancestors,
    leftSiblings: leftTail,
    focus: Relude_Tree.getValue(leftHead),
    rightSiblings: list{Relude_Tree.make(focus, children), ...rightSiblings},
    children: Relude_Tree.getChildren(leftHead),
  }, Relude_List.uncons(leftSiblings))

@ocaml.doc("
Moves to the left, unless we are already at the leftmost item
")
let moveLeftWithClamp: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.getOrElse(zipper, moveLeft(zipper))

@ocaml.doc("
Moves the focus as far as possible to the left
")
let rec moveLeftToStart: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.foldLazy(() => zipper, moveLeftToStart, moveLeft(zipper))

@ocaml.doc("
Moves left a number of times
")
let rec moveLeftTimes: 'a. (int, t<'a>) => option<t<'a>> = (times, zipper) =>
  if times < 0 {
    None
  } else if times == 0 {
    Some(zipper)
  } else {
    zipper->moveLeft->(Relude_Option.flatMap(moveLeftTimes(times - 1, _), _))
  }

@ocaml.doc("
Move the focus to the left a number of times, stopping if the leftmost sibling is reached
")
let moveLeftTimesWithClamp: 'a. (int, t<'a>) => t<'a> = (times, zipper) =>
  Relude_Option.getOrElseLazy(() => moveLeftToStart(zipper), moveLeftTimes(times, zipper))

@ocaml.doc("
Moves the focus one sibling to the right (if possible)
")
let moveRight: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}) => Relude_Option.map(((rightHead, rightTail)) => {
    ancestors,
    leftSiblings: list{Relude_Tree.make(focus, children), ...leftSiblings},
    focus: Relude_Tree.getValue(rightHead),
    rightSiblings: rightTail,
    children: Relude_Tree.getChildren(rightHead),
  }, Relude_List.uncons(rightSiblings))

@ocaml.doc("
Moves the zipper to the right one time, unless we are already at the rightmost item
")
let moveRightWithClamp: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.getOrElse(zipper, moveRight(zipper))

@ocaml.doc("
Moves the focus as far as possible to the right
")
let rec moveRightToEnd: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.foldLazy(() => zipper, moveRightToEnd, moveRight(zipper))

@ocaml.doc("
Moves right a number of times
")
let rec moveRightTimes: 'a. (int, t<'a>) => option<t<'a>> = (times, zipper) =>
  if times < 0 {
    None
  } else if times == 0 {
    Some(zipper)
  } else {
    Relude_Option.flatMap(moveRightTimes(times - 1, _), moveRight(zipper))
  }

@ocaml.doc("
Move the focus to the right a number of times, stopping if the rightmost sibling is reached
")
let moveRightTimesWithClamp: 'a. (int, t<'a>) => t<'a> = (times, zipper) =>
  Relude_Option.getOrElseLazy(() => moveRightToEnd(zipper), moveRightTimes(times, zipper))

@ocaml.doc("
Moves the focus up one level to the parent (if possible)
")
let moveUp: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}) =>
  Relude_Option.map(
    (((ancestorsHeadLeft, ancestorsHeadFocus, ancestorsHeadRight), ancestorsTail)) => {
      ancestors: ancestorsTail,
      leftSiblings: ancestorsHeadLeft,
      focus: ancestorsHeadFocus,
      rightSiblings: ancestorsHeadRight,
      children: Relude_List.flatten(list{
        Relude_List.reverse(leftSiblings),
        list{Relude_Tree.make(focus, children)},
        rightSiblings,
      }),
    },
    Relude_List.uncons(ancestors),
  )

@ocaml.doc("
Moves the zipper up a level, unless it's already at the top
")
let moveUpWithClamp: 'a. t<'a> => t<'a> = zipper => Relude_Option.getOrElse(zipper, moveUp(zipper))

@ocaml.doc("
Moves the zipper to focus the top of the tree
")
let rec moveUpToTop: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.foldLazy(() => zipper, moveUpToTop, moveUp(zipper))

@ocaml.doc("
Moves the zipper up a number of times (if possible)
")
let rec moveUpTimes: 'a. (int, t<'a>) => option<t<'a>> = (times, zipper) =>
  if times < 0 {
    None
  } else if times == 0 {
    Some(zipper)
  } else {
    Relude_Option.flatMap(moveUpTimes(times - 1, _), moveUp(zipper))
  }

@ocaml.doc("
Moves the zipper up a number of times, stopping if the top is reached
")
let moveUpTimesWithClamp: 'a. (int, t<'a>) => t<'a> = (times, zipper) =>
  Relude_Option.getOrElseLazy(() => moveUpToTop(zipper), moveUpTimes(times, zipper))

@ocaml.doc("
Moves the focus down to the first child (if possible)
")
let moveDown: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus,
  rightSiblings,
  children,
}) => Relude_Option.map(((childrenHead, childrenTail)) => {
    ancestors: list{(leftSiblings, focus, rightSiblings), ...ancestors},
    leftSiblings: list{},
    focus: Relude_Tree.getValue(childrenHead),
    rightSiblings: childrenTail,
    children: Relude_Tree.getChildren(childrenHead),
  }, Relude_List.uncons(children))

@ocaml.doc("
Moves the zipper down one time, unless there are no children
")
let moveDownWithClamp: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.getOrElse(zipper, moveDown(zipper))

@ocaml.doc("
Moves the zipper to focus the bottom of the tree (on the left-most child branches)
")
let rec moveDownToBottom: 'a. t<'a> => t<'a> = zipper =>
  Relude_Option.foldLazy(() => zipper, moveDownToBottom, moveDown(zipper))

@ocaml.doc("
Moves the focus down a number of times (if possible)
")
let rec moveDownTimes: 'a. (int, t<'a>) => option<t<'a>> = (times, zipper) =>
  if times < 0 {
    None
  } else if times == 0 {
    Some(zipper)
  } else {
    Relude_Option.flatMap(moveDownTimes(times - 1, _), moveDown(zipper))
  }

@ocaml.doc("
Moves the zipper down a number of times, stopping when we get as low as we can,
staying on the left-most child branches.
")
let moveDownTimesWithClamp: 'a. (int, t<'a>) => t<'a> = (times, zipper) =>
  Relude_Option.getOrElseLazy(() => moveDownToBottom(zipper), moveDownTimes(times, zipper))

@ocaml.doc("
Types of movements we can make in a TreeZipper
")
type movement = [
  | #Up(int)
  | #UpWithClamp(int)
  | #UpToTop
  | #Down(int)
  | #DownWithClamp(int)
  | #DownToBottom
  | #Left(int)
  | #LeftWithClamp(int)
  | #LeftToStart
  | #Right(int)
  | #RightWithClamp(int)
  | #RightToEnd
]

@ocaml.doc("
Applies a single movement command to a zipper
")
let moveOnceBy: 'a. (movement, t<'a>) => option<t<'a>> = (move, zipper) =>
  switch move {
  | #Up(n) => moveUpTimes(n, zipper)
  | #UpWithClamp(n) => Some(moveUpTimesWithClamp(n, zipper))
  | #UpToTop => Some(moveUpToTop(zipper))
  | #Down(n) => moveDownTimes(n, zipper)
  | #DownWithClamp(n) => Some(moveDownTimesWithClamp(n, zipper))
  | #DownToBottom => Some(moveDownToBottom(zipper))
  | #Left(n) => moveLeftTimes(n, zipper)
  | #LeftWithClamp(n) => Some(moveLeftTimesWithClamp(n, zipper))
  | #LeftToStart => Some(moveLeftToStart(zipper))
  | #Right(n) => moveRightTimes(n, zipper)
  | #RightWithClamp(n) => Some(moveRightTimesWithClamp(n, zipper))
  | #RightToEnd => Some(moveRightToEnd(zipper))
  }

@ocaml.doc("
Applies a list of movement commands to a zipper
")
let moveBy: 'a. (list<movement>, t<'a>) => option<t<'a>> = (moves, zipper) =>
  Relude_List.foldLeft(
    (zipperOpt, move) => Relude_Option.flatMap(moveOnceBy(move, ...), zipperOpt),
    Some(zipper),
    moves,
  )

@ocaml.doc("
Applies a list of movement commands to a zipper and collects an accumulated value when visiting each new focus
")
let foldBy: 'a 'b. (list<movement>, ('b, 'a) => 'b, 'b, t<'a>) => option<(t<'a>, 'b)> = (
  moves,
  f,
  init,
  zipper,
) =>
  Relude_List.foldLeft(
    (zipperAccOpt, move) =>
      Relude_Option.flatMap(
        ((zipper, acc)) =>
          Relude_Option.map(
            nextZipper => (nextZipper, f(acc, getFocusValue(nextZipper))),
            moveOnceBy(move, zipper),
          ),
        zipperAccOpt,
      ),
    Some((zipper, init)),
    moves,
  )

@ocaml.doc("
Converts a zipper of 'a to a zipper of 'b using a pure function
")
let map: 'a 'b. (. 'a => 'b, t<'a>) => t<'b> = (
  aToB,
  {ancestors, leftSiblings, focus, rightSiblings, children},
) => {
  ancestors: Relude_List.map(
    ((left, focus, right)) => (
      Relude_List.map(x => Relude_Tree.map(aToB, x, ...), left),
      aToB(focus),
      Relude_List.map(x => Relude_Tree.map(aToB, x, ...), right),
    ),
    ancestors,
  ),
  leftSiblings: Relude_List.map(x => Relude_Tree.map(aToB, x, ...), leftSiblings),
  focus: aToB(focus),
  rightSiblings: Relude_List.map(x => Relude_Tree.map(aToB, x, ...), rightSiblings),
  children: Relude_List.map(x => Relude_Tree.map(aToB, x, ...), children),
}

module Functor: BsBastet.Interface.FUNCTOR with type t<'a> = t<'a> = {
  type t<'a> = t<'a>
  let map = map
}
include Relude_Extensions_Functor.FunctorExtensions(Functor)

// TODO: could implement Apply/Applicative/Monad/Comonad/etc.

let findInFocus: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, zipper) =>
  pred(getFocusValue(zipper)) ? Some(zipper) : None

@ocaml.doc("
Finds a value in the curernt focus and recursively the children.
Equivalent to a depth first search in the currently focused tree.
")
let findInFocusAndChildren: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, zipper) => {
  let rec dfs = zipper =>
    Relude_Option.orElseLazy(
      ~fallback=() => Relude_Option.flatMap(dfs, moveRight(zipper)),
      Relude_Option.orElseLazy(
        ~fallback=() => Relude_Option.flatMap(dfs, moveDown(zipper)),
        findInFocus(pred, zipper),
      ),
    )

  Relude_Option.orElseLazy(
    ~fallback=() => Relude_Option.flatMap(dfs, moveDown(zipper)),
    findInFocus(pred, zipper),
  )
}

@ocaml.doc("
Attempts to find a value by searching the current focus and left siblings
")
let rec findLeft: 'a. (~checkFocus: bool=?, 'a => bool, t<'a>) => option<t<'a>> = (
  ~checkFocus=true,
  pred,
  zipper,
) =>
  if checkFocus {
    Relude_Option.orElseLazy(
      ~fallback=() => Relude_Option.flatMap(findLeft(pred, ...), moveLeft(zipper)),
      findInFocusAndChildren(pred, zipper),
    )
  } else {
    Relude_Option.flatMap(findLeft(pred, ...), moveLeft(zipper))
  }

@ocaml.doc("
Attempts to find a value by searching the current focus and right siblings
")
and findRight: 'a. (~checkFocus: bool=?, 'a => bool, t<'a>) => option<t<'a>> = (
  ~checkFocus=true,
  pred,
  zipper,
) =>
  if checkFocus {
    Relude_Option.orElseLazy(
      ~fallback=() => Relude_Option.flatMap(findRight(pred, ...), moveRight(zipper)),
      findInFocusAndChildren(pred, zipper),
    )
  } else {
    Relude_Option.flatMap(findRight(pred, ...), moveRight(zipper))
  }

@ocaml.doc("
Attempts to find a value by searching the current focus, then the left siblings
from the focus outward, then the right siblings from the focus outward.
")
and findLeftOrRight: 'a. (~checkFocus: bool=?, 'a => bool, t<'a>) => option<t<'a>> = (
  ~checkFocus=true,
  pred,
  zipper,
) =>
  Relude_Option.orElseLazy(
    ~fallback=() => findRight(~checkFocus=false, pred, zipper),
    findLeft(~checkFocus, pred, zipper),
  )

@ocaml.doc("
Attempts to find a value by moving up a level, then searching left and right on the parent level,
then progressing upward.
")
and findUp: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, zipper) =>
  Relude_Option.flatMap(parentZipper =>
    // TODO: I think this is repeatedly searching some of the same values as we move up
    Relude_Option.orElseLazy(
      ~fallback=() => findUp(pred, parentZipper),
      findLeftOrRight(pred, parentZipper),
    )
  , moveUp(zipper))

@ocaml.doc("
Attempts to find a value by moving down a level, then searching left and right on the child level,
then progressing downward.
")
and findDown: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, zipper) =>
  Relude_Option.flatMap(
    childZipper =>
      Relude_Option.orElseLazy(
        ~fallback=() => findDown(pred, childZipper),
        findRight(pred, childZipper),
      ),
    moveDown(zipper),
  )

@ocaml.doc("
Attempts to find a value anywhere in the zipper, left/right/up/down
")
and find: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, zipper) =>
  Relude_Option.orElseLazy(
    ~fallback=() => findDown(pred, zipper),
    Relude_Option.orElseLazy(~fallback=() => findUp(pred, zipper), findLeftOrRight(pred, zipper)),
  )

@ocaml.doc("
Inserts a new tree, and pushes the current focus to the left
")
let insertTreeWithPushLeft: 'a. (Relude_Tree.t<'a>, t<'a>) => option<t<'a>> = (
  newTree,
  {ancestors, leftSiblings, focus, rightSiblings, children},
) =>
  if Relude_List.isNotEmpty(ancestors) {
    Some({
      ancestors,
      leftSiblings: list{{value: focus, children}, ...leftSiblings},
      focus: Relude_Tree.getValue(newTree),
      rightSiblings,
      children: Relude_Tree.getChildren(newTree),
    })
  } else {
    None
  }

@ocaml.doc("
Inserts a new value (singleton tree), and pushes the current focus to the left
")
let insertWithPushLeft: 'a. ('a, t<'a>) => option<t<'a>> = (newFocus, tree) =>
  insertTreeWithPushLeft(Relude_Tree.pure(newFocus), tree)

@ocaml.doc("
Inserts a new tree, and pushes the current focus to the right
")
let insertTreeWithPushRight: 'a. (Relude_Tree.t<'a>, t<'a>) => option<t<'a>> = (
  newTree,
  {ancestors, leftSiblings, focus, rightSiblings, children},
) =>
  if Relude_List.isNotEmpty(ancestors) {
    Some({
      ancestors,
      leftSiblings,
      focus: Relude_Tree.getValue(newTree),
      rightSiblings: list{{value: focus, children}, ...rightSiblings},
      children: Relude_Tree.getChildren(newTree),
    })
  } else {
    None
  }

@ocaml.doc("
Inserts a new value (singleton tree), and pushes the current focus to the right
")
let insertWithPushRight: 'a. ('a, t<'a>) => option<t<'a>> = (newFocus, tree) =>
  insertTreeWithPushRight(Relude_Tree.pure(newFocus), tree)

@ocaml.doc("
Deletes the tree at the focus, and pulls the left sibling into focus (if possible)
")
let deleteWithPullLeft: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus: _,
  rightSiblings,
  children: _,
}) =>
  if Relude_List.isNotEmpty(ancestors) {
    Relude_Option.map(((leftHead, leftTail)) => {
      ancestors,
      leftSiblings: leftTail,
      focus: Relude_Tree.getValue(leftHead),
      rightSiblings,
      children: Relude_Tree.getChildren(leftHead),
    }, Relude_List.uncons(leftSiblings))
  } else {
    None
  }

@ocaml.doc("
Deletes the tree at the focus, and pulls the right sibling into focus (if possible)
")
let deleteWithPullRight: 'a. t<'a> => option<t<'a>> = ({
  ancestors,
  leftSiblings,
  focus: _,
  rightSiblings,
  children: _,
}) =>
  if Relude_List.isNotEmpty(ancestors) {
    Relude_Option.map(((rightHead, rightTail)) => {
      ancestors,
      leftSiblings,
      focus: Relude_Tree.getValue(rightHead),
      rightSiblings: rightTail,
      children: Relude_Tree.getChildren(rightHead),
    }, Relude_List.uncons(rightSiblings))
  } else {
    None
  }

@ocaml.doc("
Attempts to delete by deleting and pulling from the left.  If there is no item on the left,
it tries to pull from the right.  If there is no item on the right, it moves the focus up a level,
discarding the current focus and children.
")
let delete: 'a. t<'a> => option<t<'a>> = zipper =>
  Relude_Option.orElseLazy(
    ~fallback=() =>
      Relude_Option.map(
        (((parentLeftSiblings, parentFocus, parentRightSiblings), ancestorsTail)) => {
          ancestors: ancestorsTail,
          leftSiblings: parentLeftSiblings,
          focus: parentFocus,
          rightSiblings: parentRightSiblings,
          children: list{},
        },
        Relude_List.uncons(getAncestors(zipper)),
      ),
    Relude_Option.orElseLazy(
      ~fallback=() => deleteWithPullRight(zipper),
      deleteWithPullLeft(zipper),
    ),
  )

let showBy: 'a. ('a => string, t<'a>) => string = (
  showA,
  {ancestors, leftSiblings, focus, rightSiblings, children},
) => {
  let ancestorsStr = Relude_List.showBy(
    Relude_Tuple.showBy3(
      Relude_List.showBy(Relude_Tree.showBy(showA, ...), ...),
      showA,
      Relude_List.showBy(Relude_Tree.showBy(showA, ...), ...),
      ...
    ),
    ancestors,
  )

  let leftSiblingsStr = Relude_List.showBy(Relude_Tree.showBy(showA, ...), leftSiblings)

  let focusStr = showA(focus)

  let rightSiblingsStr = Relude_List.showBy(Relude_Tree.showBy(showA, ...), rightSiblings)

  let childrenStr = Relude_List.showBy(Relude_Tree.showBy(showA, ...), children)

  "TreeZipper" ++
  ("\n" ++
  ("ancestors = " ++
  (ancestorsStr ++
  ("\n" ++
  ("leftSiblings = " ++
  (leftSiblingsStr ++
  ("\n" ++
  ("focus = " ++
  (focusStr ++
  ("\n" ++
  ("rightSiblings = " ++
  (rightSiblingsStr ++ ("\n" ++ ("children = " ++ childrenStr))))))))))))))
}
