@@uncurried
@@uncurried.swap

open BsBastet.Interface

@ocaml.doc("
A non-empty multi-way (aka \"rose\") tree which contains a \"top\" value and
a list of child trees.
")
type rec t<'a> = {
  value: 'a,
  children: list<t<'a>>,
}

@ocaml.doc("
Constructs a tree containing a single value
")
let pure: 'a. 'a => t<'a> = a => {value: a, children: list{}}

@ocaml.doc("
Alias for pure
")
let singleton = pure

@ocaml.doc("
Checks if this tree contains a single item
")
let isSingleton: 'a. t<'a> => bool = ({value: _, children}) => Relude_List.isEmpty(children)

@ocaml.doc("
Constructs a tree from a value and children
")
let make: 'a. ('a, list<t<'a>>) => t<'a> = (value, children) => {value, children}

@ocaml.doc("
Destructures a tree into a tuple of the value and children
")
let unmake: 'a. t<'a> => ('a, list<t<'a>>) = ({value, children}) => (value, children)

@ocaml.doc("
Creates a tree using a seed value and a function to produce child seed values
")
let rec fill: 'a. ('a => list<'a>, 'a) => t<'a> = (getChildrenSeeds, seed) => {
  value: seed,
  children: seed->getChildrenSeeds->(Relude_List.map(fill(getChildrenSeeds, ...), _)),
}

@ocaml.doc("
Gets the top value of the tree
")
let getValue: 'a. t<'a> => 'a = ({value, children: _}) => value

@ocaml.doc("
Replaces only the top value of the tree
")
let setValue: 'a. ('a, t<'a>) => t<'a> = (newValue, {value: _, children}) => {
  value: newValue,
  children,
}

@ocaml.doc("
Updates only the top value of the tree
")
let modifyValue: 'a. ('a => 'a, t<'a>) => t<'a> = (f, {value, children}) => {
  value: f(value),
  children,
}

@ocaml.doc("
Gets the list of child trees
")
let getChildren: 'a. t<'a> => list<t<'a>> = ({value: _, children}) => children

@ocaml.doc("
Gets the child at the given index
")
let getChildAt: 'a. (int, t<'a>) => option<t<'a>> = (index, {value: _, children}) =>
  children->(Relude_List.at(index, _))

@ocaml.doc("
Replaces all the children with the given list of new children
")
let setChildren: 'a. (list<t<'a>>, t<'a>) => t<'a> = (newChildren, {value, children: _}) => {
  value,
  children: newChildren,
}

@ocaml.doc("
Modifies the children using the given function
")
let modifyChildren: 'a. (list<t<'a>> => list<t<'a>>, t<'a>) => t<'a> = (f, {value, children}) => {
  value,
  children: f(children),
}

@ocaml.doc("
Adds a child on the left side
")
let prependChild: 'a. (~child: t<'a>, t<'a>) => t<'a> = (~child, {value, children}) => {
  value,
  children: Relude_List.prepend(child, children),
}

@ocaml.doc("
Adds a child on the right side
")
let appendChild: 'a. (~child: t<'a>, t<'a>) => t<'a> = (~child, {value, children}) => {
  value,
  children: Relude_List.append(child, children),
}

@ocaml.doc("
Prepends new children to the left side of the existing children
")
let prependChildren: 'a. (list<t<'a>>, t<'a>) => t<'a> = (newChildren, {value, children}) => {
  value,
  children: Relude_List.concat(newChildren, children),
}

@ocaml.doc("
Appends new children to the right side of the existing children
")
let appendChildren: 'a. (list<t<'a>>, t<'a>) => t<'a> = (newChildren, {value, children}) => {
  value,
  children: Relude_List.concat(children, newChildren),
}

@ocaml.doc("
Dumps the tree to a flattened non-empty list, with the top value first, proceeding downward and left-to-right
")
let toNonEmptyList: 'a. t<'a> => Relude_NonEmpty.List.t<'a> = ({value, children}) => {
  let rec flatten' = ({value, children}) =>
    switch children {
    | list{} => list{value}
    | children => list{value, ...Relude_List.flatMap(flatten', children)}
    }
  Relude_NonEmpty.List.make(value, Relude_List.flatMap(flatten', children))
}

@ocaml.doc("
Dumps the tree to a flattened non-empty array, with the top value first, proceeding downward and left-to-right
")
let toNonEmptyArray: 'a. t<'a> => Relude_NonEmpty.Array.t<'a> = tree =>
  tree->toNonEmptyList->Relude_NonEmpty.Array.fromNonEmptyList

@ocaml.doc("
Zips two tree together position-by-position using the given function
")
let rec zipWith: 'a 'b 'c. (('a, 'b) => 'c, t<'a>, t<'b>) => t<'c> = (
  f,
  {value: valueA, children: childrenA},
  {value: valueB, children: childrenB},
) => {
  value: f(valueA, valueB),
  children: Relude_List.zipWith(zipWith(f, ...), childrenA, childrenB),
}

@ocaml.doc("
Zips two tree together position-by-position to make a tree of tuples
")
let zip: 'a 'b. (t<'a>, t<'b>) => t<('a, 'b)> = (ta, tb) => zipWith((a, b) => (a, b), ta, tb)

@ocaml.doc("
Maps a function over all values of the tree
")
let rec map: 'a 'b. (. 'a => 'b, t<'a>) => t<'b> = (aToB, {value, children}) => {
  value: aToB(value),
  children: children->(Relude_List.map(map(aToB, _), _)),
}

module Functor: FUNCTOR with type t<'a> = t<'a> = {
  type t<'a> = t<'a>
  let map = map
}
include Relude_Extensions_Functor.FunctorExtensions(Functor)

@ocaml.doc("
Applies a tree of functions to a tree of values position-by-position, trimming off non-matching branches.
")
let rec apply: 'a 'b. (. t<'a => 'b>, t<'a>) => t<'b> = (
  {value: aToB, children: aToBChildTrees},
  {value: a, children: aChildTrees} as aTree,
) => {
  value: aToB(a),
  children: Relude_List.concat(
    aChildTrees->(List.map(aChildTree => map(aToB, aChildTree), _)),
    aToBChildTrees->(Relude_List.map(aToBChildTree => apply(aToBChildTree, aTree), _)),
  ),
}

module Apply: APPLY with type t<'a> = t<'a> = {
  include Functor
  let apply = apply
}
include Relude_Extensions_Apply.ApplyExtensions(Apply)

module Applicative: APPLICATIVE with type t<'a> = t<'a> = {
  include Apply
  let pure = pure
}
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative)

let rec bind: 'a 'b. (. t<'a>, 'a => t<'b>) => t<'b> = (
  {value: valueA, children: childTreesA},
  aToTreeB,
) => {
  let {value: valueB, children: childTreesB} = valueA->aToTreeB
  let otherChildTreesB = childTreesA->(Relude_List.map(childTreeA => bind(childTreeA, aToTreeB), _))
  {
    value: valueB,
    children: Relude_List.concat(childTreesB, otherChildTreesB),
  }
}

module Monad: MONAD with type t<'a> = t<'a> = {
  include Applicative
  let flat_map = bind
}
include Relude_Extensions_Monad.MonadExtensions(Monad)

let rec extend: 'a 'b. (t<'a> => 'b, t<'a>) => t<'b> = (
  treeAToB,
  {value: _, children: childrenA} as treeA,
) => {
  value: treeA->treeAToB,
  children: childrenA->(Relude_List.map(extend(treeAToB, _), _)),
}

module Extend: EXTEND with type t<'a> = t<'a> = {
  include Functor
  let extend = extend
}
include Relude_Extensions_Extend.ExtendExtensions(Extend)

module Comonad: COMONAD with type t<'a> = t<'a> = {
  include Extend
  let extract = getValue
}
include Relude_Extensions_Comonad.ComonadExtensions(Comonad)

@ocaml.doc("
Folds a tree from left to right, depth first
")
let rec foldLeft: 'a 'b. (('b, 'a) => 'b, 'b, t<'a>) => 'b = (f, init, {value, children}) => {
  let acc = children->(Relude_List.foldLeft((acc, child) => foldLeft(f, acc, child), init, _))
  f(acc, value)
}

@ocaml.doc("
Folds a tree from right to left, depth first
")
let rec foldRight: 'a 'b. (('a, 'b) => 'b, 'b, t<'a>) => 'b = (f, init, {value, children}) => {
  let acc = children->(Relude_List.foldRight((child, acc) => foldRight(f, acc, child), init, _))
  f(value, acc)
}

module Foldable: FOLDABLE with type t<'a> = t<'a> = {
  type t<'a> = t<'a>
  let fold_left = foldLeft
  let fold_right = foldRight

  module Fold_Map = (M: MONOID) => {
    let fold_map: ('a => M.t, t<'a>) => M.t = (f, tree) =>
      tree->(foldLeft((acc, value) => M.append(acc, f(value)), M.empty, _))
  }

  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map: 'a 'b. ('a => M.t<'b>, t<'a>) => M.t<'b> = (f, tree) =>
      tree->(foldLeft((acc, value) => M.append(acc, f(value)), M.empty, _))
  }

  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map: 'a 'b. ('a => P.t<'b>, t<'a>) => P.t<'b> = (f, tree) =>
      tree->(foldLeft((acc, value) => P.alt(acc, f(value)), P.empty, _))
  }
}
include Relude_Extensions_Foldable.FoldableExtensions(Foldable)

let rec unfold: 'a. ('a => option<('a, 'a)>, 'a) => t<'a> = (f, init) =>
  switch f(init) {
  | Some((a, next)) => {value: a, children: list{unfold(f, next)}}
  | None => pure(init)
  }

module Unfoldable: UNFOLDABLE with type t<'a> = t<'a> = {
  type t<'a> = t<'a>
  let unfold = unfold
}
include Relude_Extensions_Unfoldable.UnfoldableExtensions(Unfoldable)

module WithApplicative = (A: APPLICATIVE) => {
  module Traversable: TRAVERSABLE with type t<'a> = t<'a> and type applicative_t<'a> = A.t<'a> = {
    type t<'a> = t<'a>
    type applicative_t<'a> = A.t<'a>
    include (Functor: FUNCTOR with type t<'a> := t<'a>)
    include (Foldable: FOLDABLE with type t<'a> := t<'a>)
    module ListTraversable = Relude_List.Traversable(A)

    let rec traverse: ('a => A.t<'b>, t<'a>) => A.t<t<'b>> = (f, {value, children}) => {
      let \"<$>" = A.map
      let \"<*>" = A.apply
      \"<*>"(
        \"<$>"(x => make(x, _), f(value)),
        ListTraversable.traverse(x => traverse(f, x), children),
      )
    }

    let sequence: 'a. t<A.t<'a>> => A.t<t<'a>> = tree => traverse(a => a, tree)
  }
}

@ocaml.doc("
Filters the tree to only contain values which pass the given predicate. If a value
doesn't pass the predicate, the entire subtree is trimmed.
")
let rec filter: 'a. ('a => bool, t<'a>) => option<t<'a>> = (pred, {value, children}) =>
  if pred(value) {
    Some({
      value,
      children: children->(Relude_List.mapOption(x => filter(pred, x), _)),
    })
  } else {
    None
  }

@ocaml.doc("
Shows a tree using the given function for the contained values
")
let rec showBy: 'a. ('a => string, t<'a>) => string = (showA, {value, children}) =>
  "Tree " ++ (showA(value) ++ (" " ++ Relude_List.showBy(showBy(showA, ...), children)))

module type SHOW_F = (ShowA: SHOW) => (SHOW with type t = t<ShowA.t>)

module Show: SHOW_F = (ShowA: SHOW) => {
  type t = t<ShowA.t>
  let show = tree => showBy(ShowA.show, tree)
}

let showPrettyBy: 'a. ('a => string, t<'a>) => string = (showA, tree) => {
  let rec showPrettyByWithIndent: 'a. (int, 'a => string, t<'a>) => string = (
    level,
    showA,
    tree,
  ) => {
    let indent = if level > 0 {
      Relude_String.repeat(level - 1, "   ") ++ "|- "
    } else {
      ""
    }
    let childrenStr =
      tree
      ->getChildren
      ->Relude_List.map(x => showPrettyByWithIndent(level + 1, showA, x), _)
      ->(Relude_List.String.joinWith("", _))
    indent ++ (showA(tree->getValue) ++ ("\n" ++ childrenStr))
  }
  showPrettyByWithIndent(0, showA, tree)
}

module ShowPretty: SHOW_F = (ShowA: SHOW) => {
  type t = t<ShowA.t>
  let show = showPrettyBy(ShowA.show, ...)
}

@ocaml.doc("
Compares two trees for quality using the given function
")
let rec eqBy: 'a. (('a, 'a) => bool, t<'a>, t<'a>) => bool = (
  eqA,
  {value: value1, children: children1},
  {value: value2, children: children2},
) => eqA(value1, value2) && Relude_List.eqBy(eqBy(eqA, ...), children1, children2)

module type EQ_F = (EqA: EQ) => (EQ with type t = t<EqA.t>)

module Eq: EQ_F = (EqA: EQ) => {
  type t = t<EqA.t>
  let eq = (tree1, tree2) => eqBy(EqA.eq, tree1, tree2)
}
