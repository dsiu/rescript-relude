@@uncurried
@@uncurried.swap

@ocaml.doc("
Type of an empty HList
")
type nil = unit

@ocaml.doc("
Type of a non-empty HList
")
type cons<'h, 't> = 'h => 't

@ocaml.doc("
[HList.t] is a heterogenous list type which can store a list of differently-
typed values while retaining full type safety.

Originally this was using the special constructors [[]] and [::], but these seem
to constantly conflict with the list versions in Pervasives.
")
type rec t<'l> =
  //| []: t(nil)
  //| ::('h, t('t)) : t(cons('h, 't));
  | HNil: t<nil>
  | HCons('h, t<'t>): t<cons<'h, 't>>

@ocaml.doc("
[HList.empty] is the empty instance of an [HList]
")
let empty: t<nil> = HNil

@ocaml.doc("
[HList.pure] lifts a pure value into a singleton HLlist.
")
let pure: 'a => t<cons<'a, unit>> = a => HCons(a, HNil)

@ocaml.doc("
Creates an HList from head and tail parts.
")
let cons: ('h, t<'t>) => t<cons<'h, 't>> = (h, t) => HCons(h, t)

@ocaml.doc("
Splits an HList into head and tail parts.
")
let uncons: t<cons<'h, 't>> => ('h, t<'t>) = x =>
  switch x {
  | HCons(h, t) => (h, t)
  }

@ocaml.doc("
Returns the first element in a HList of 1 or more elements.
")
let head: t<cons<'h, _>> => 'h = x =>
  switch x {
  | HCons(h, _) => h
  }

@ocaml.doc("
Returns the tail of an HList of 1 or more elements.
")
let tail: t<cons<_, 't>> => t<'t> = x =>
  switch x {
  | HCons(_, t) => t
  }

@ocaml.doc("
Returns the second element of an HList of at least 2 elements
")
let second: t<cons<_, cons<'b, _>>> => 'b = x =>
  switch x {
  | HCons(_, HCons(x, _)) => x
  }

@ocaml.doc("
Returns the third element of an HList of at least 3 elements
")
let third: t<cons<_, cons<_, cons<'c, _>>>> => 'c = x =>
  switch x {
  | HCons(_, HCons(_, HCons(x, _))) => x
  }

@ocaml.doc("
Returns the fourth element of an HList of at least 4 elements
")
let fourth: t<cons<_, cons<_, cons<_, cons<'d, _>>>>> => 'd = x =>
  switch x {
  | HCons(_, HCons(_, HCons(_, HCons(x, _)))) => x
  }

@ocaml.doc("
Returns the fifth element of an HList of at least 5 elements
")
let fifth: t<cons<_, cons<_, cons<_, cons<_, cons<'e, _>>>>>> => 'e = x =>
  switch x {
  | HCons(_, HCons(_, HCons(_, HCons(_, HCons(x, _))))) => x
  }

@ocaml.doc("
Creates an HList from a 2-tuple
")
let fromTuple2: (('a, 'b)) => t<cons<'a, cons<'b, nil>>> = ((a, b)) => HCons(a, HCons(b, HNil))

@ocaml.doc("
Creates an HList from a 3-tuple
")
let fromTuple3: (('a, 'b, 'c)) => t<cons<'a, cons<'b, cons<'c, nil>>>> = ((a, b, c)) => HCons(
  a,
  HCons(b, HCons(c, HNil)),
)

@ocaml.doc("
Creates an HList from a 4-tuple
")
let fromTuple4: (('a, 'b, 'c, 'd)) => t<cons<'a, cons<'b, cons<'c, cons<'d, nil>>>>> = ((
  a,
  b,
  c,
  d,
)) => HCons(a, HCons(b, HCons(c, HCons(d, HNil))))

@ocaml.doc("
Creates an HList from a 5-tuple
")
let fromTuple5: (('a, 'b, 'c, 'd, 'e)) => t<
  cons<'a, cons<'b, cons<'c, cons<'d, cons<'e, nil>>>>>,
> = ((a, b, c, d, e)) => HCons(a, HCons(b, HCons(c, HCons(d, HCons(e, HNil)))))

@ocaml.doc("
Converts an HList of 2 elements to a tuple
")
let toTuple2: t<cons<'a, cons<'b, nil>>> => ('a, 'b) = (HCons(a, HCons(b, HNil))) => (a, b)

@ocaml.doc("
Converts an HList of 3 elements to a tuple
")
let toTuple3: t<cons<'a, cons<'b, cons<'c, nil>>>> => ('a, 'b, 'c) = (HCons(
  a,
  HCons(b, HCons(c, HNil)),
)) => (a, b, c)

@ocaml.doc("
Converts an HList of 4 elements to a tuple
")
let toTuple4: t<cons<'a, cons<'b, cons<'c, cons<'d, nil>>>>> => ('a, 'b, 'c, 'd) = (HCons(
  a,
  HCons(b, HCons(c, HCons(d, HNil))),
)) => (a, b, c, d)

@ocaml.doc("
Converts an HList of 4 elements to a tuple
")
let toTuple5: t<cons<'a, cons<'b, cons<'c, cons<'d, cons<'e, nil>>>>>> => (
  'a,
  'b,
  'c,
  'd,
  'e,
) = (HCons(a, HCons(b, HCons(c, HCons(d, HCons(e, HNil)))))) => (a, b, c, d, e)
