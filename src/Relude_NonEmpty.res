@@uncurried
@@uncurried.swap

open BsBastet.Interface

@ocaml.doc("
Creates a NonEmpty module with the given SEQUENCE module to handle the tail part.
")
module WithSequence = (TailSequence: Relude_Interface.SEQUENCE) => {
  @ocaml.doc("
  The type of a non-empty sequence of values
  ")
  type t<'a> = NonEmpty('a, TailSequence.t<'a>)

  @ocaml.doc("
  Creates a NonEmpty with a single value
  ")
  let one: 'a. 'a => t<'a> = head => NonEmpty(head, TailSequence.emptyLazy())

  @ocaml.doc("
  Constructs a NonEmpty with the given head and tail values
  ")
  let make: 'a. ('a, TailSequence.t<'a>) => t<'a> = (head, tailSequence) => NonEmpty(
    head,
    tailSequence,
  )

  @ocaml.doc("
  Converts the non-empty Sequence into a NonEmpty value. This operation can
  fail with None if the Sequence is empty.
  ")
  let fromSequence: 'a. TailSequence.t<'a> => option<t<'a>> = sequence =>
    TailSequence.head(sequence)->Belt.Option.map(head => NonEmpty(
      head,
      TailSequence.tailOrEmpty(sequence),
    ))

  @ocaml.doc("
  Converts the NonEmpty value into a Sequence. This operation cannot fail.
  ")
  let toSequence: 'a. t<'a> => TailSequence.t<'a> = x =>
    switch x {
    | NonEmpty(head, tail) => TailSequence.concat(TailSequence.Monad.pure(head), tail)
    }

  @ocaml.doc("
  Converts a list to a NonEmpty, failing if the list is empty
  ")
  let fromList: 'a. list<'a> => option<t<'a>> = list =>
    switch list {
    | list{} => None
    | list{h, ...t} => Some(NonEmpty(h, TailSequence.fromList(t)))
    }

  @ocaml.doc("
  Converts an array to a NonEmpty, failing if the list is empty
  ")
  let fromArray: 'a. array<'a> => option<t<'a>> = array =>
    Relude_Array_Base.uncons(array)->(
      Relude_Option_Instances.map(((h, t)) => NonEmpty(h, TailSequence.fromArray(t)), _)
    )

  @ocaml.doc("
  Prepends a new head value to the NonEmpty
  ")
  let cons: 'a. ('a, t<'a>) => t<'a> = (head, tailNonEmpty) => NonEmpty(
    head,
    toSequence(tailNonEmpty),
  )

  @ocaml.doc("
  Splits the NonEmpty into head and tail parts. This operation cannot
  fail because we are guaranteed to have a head value.
  ")
  let uncons: 'a. t<'a> => ('a, TailSequence.t<'a>) = x =>
    switch x {
    | NonEmpty(head, tail) => (head, tail)
    }

  @ocaml.doc("
  Gets the head value from the NonEmpty. This operation cannot fail because
  we are guaranteed to have a head value.
  ")
  let head: 'a. t<'a> => 'a = x =>
    switch x {
    | NonEmpty(head, _) => head
    }

  @ocaml.doc("
  Gets the tail of a NonEmpty. The tail of a NonEmpty can be an empty sequence.
  ")
  let tail: 'a. t<'a> => TailSequence.t<'a> = x =>
    switch x {
    | NonEmpty(_, tail) => tail
    }

  @ocaml.doc("
  Concatenates two NonEmpty values, with the left side first and the right side last.
  ")
  let concat: 'a. (t<'a>, t<'a>) => t<'a> = (nonEmpty1, nonEmpty2) => NonEmpty(
    head(nonEmpty1),
    TailSequence.concat(tail(nonEmpty1), toSequence(nonEmpty2)),
  )

  module SemigroupAny: SEMIGROUP_ANY with type t<'a> = t<'a> = {
    type t<'a> = t<'a>
    let append = concat
  }
  include Relude_Extensions_SemigroupAny.SemigroupAnyExtensions(SemigroupAny)

  module MagmaAny: MAGMA_ANY with type t<'a> = t<'a> = {
    type t<'a> = t<'a>
    let append = concat
  }

  @ocaml.doc("
  Reduces the NonEmpty to a single accumulator value, using the head as the initial
  accumulator, and running the function for the remaining items.
  ")
  let reduceLeft: 'a. (('a, 'a) => 'a, t<'a>) => 'a = (f, NonEmpty(x, xs)) =>
    TailSequence.Foldable.fold_left(f, x, xs)

  @ocaml.doc("
  Folds the NonEmpty to a single accumulator value, using the given starter value.
  ")
  let foldLeft: 'a 'b. (('b, 'a) => 'b, 'b, t<'a>) => 'b = (f, init, NonEmpty(x, xs)) =>
    TailSequence.Foldable.fold_left(f, f(init, x), xs)

  @ocaml.doc("
  Folds the NonEmpty to a single accumulator value, using the given starter value.
  ")
  let foldRight: 'a 'b. (('a, 'b) => 'b, 'b, t<'a>) => 'b = (f, init, NonEmpty(x, xs)) =>
    f(x, TailSequence.Foldable.fold_right(f, init, xs))

  module Foldable: FOLDABLE with type t<'a> = t<'a> = {
    type t<'a> = t<'a>

    let fold_left = foldLeft

    let fold_right = foldRight

    module Fold_Map = (FoldMapMonoid: MONOID) => {
      module TailFoldMap = TailSequence.Foldable.Fold_Map(FoldMapMonoid)
      let fold_map: 'a. ('a => FoldMapMonoid.t, t<'a>) => FoldMapMonoid.t = (f, NonEmpty(x, xs)) =>
        FoldMapMonoid.append(f(x), TailFoldMap.fold_map(f, xs))
    }

    module Fold_Map_Plus = (FoldMapPlus: PLUS) => {
      module TailFoldMapPlus = TailSequence.Foldable.Fold_Map_Plus(FoldMapPlus)
      let fold_map: 'a 'b. ('a => FoldMapPlus.t<'b>, t<'a>) => FoldMapPlus.t<'b> = (
        f,
        NonEmpty(x, xs),
      ) => FoldMapPlus.alt(f(x), TailFoldMapPlus.fold_map(f, xs))
    }

    module Fold_Map_Any = (FoldMapAny: MONOID_ANY) => {
      module SequenceFoldMapAny = TailSequence.Foldable.Fold_Map_Any(FoldMapAny)
      let fold_map: 'a 'b. ('a => FoldMapAny.t<'b>, t<'a>) => FoldMapAny.t<'b> = (
        f,
        NonEmpty(x, xs),
      ) => FoldMapAny.append(f(x), SequenceFoldMapAny.fold_map(f, xs))
    }
  }
  include Relude_Extensions_Foldable.FoldableExtensions(Foldable)

  @ocaml.doc("
  Maps a pure function over the NonEmpty
  ")
  let map: 'a 'b. (. 'a => 'b, t<'a>) => t<'b> = (f, NonEmpty(x, xs)) => NonEmpty(
    f(x),
    TailSequence.Monad.map(f, xs),
  )

  module Functor: FUNCTOR with type t<'a> = t<'a> = {
    type t<'a> = t<'a>
    let map = map
  }
  include Relude_Extensions_Functor.FunctorExtensions(Functor)

  @ocaml.doc("
  Flattens a nested NonEmpty value one time
  ")
  let flatten: 'a. t<t<'a>> => t<'a> = nonEmpty => reduceLeft(concat, nonEmpty)

  @ocaml.doc("
  Applies a NonEmpty sequence of function to a NonEmpty sequence of values
  ")
  let apply: 'a 'b. (. t<'a => 'b>, t<'a>) => t<'b> = (ff, fa) => map(f => map(f, fa), ff)->flatten

  module Apply: APPLY with type t<'a> = t<'a> = {
    include Functor
    let apply = apply
  }
  include Relude_Extensions_Apply.ApplyExtensions(Apply)

  @ocaml.doc("
  Lifts a single pure value into a NonEmpty of one item

  Alias for [one]
  ")
  let pure = one

  module Applicative: APPLICATIVE with type t<'a> = t<'a> = {
    include Apply
    let pure = pure
  }
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative)

  @ocaml.doc("
  Applies a monadic function to a NonEmpty sequence of values
  ")
  let bind: 'a 'b. (. t<'a>, 'a => t<'b>) => t<'b> = (nonEmpty, f) => map(f, nonEmpty)->flatten

  module Monad: MONAD with type t<'a> = t<'a> = {
    include Applicative
    let flat_map = bind
  }
  include Relude_Extensions_Monad.MonadExtensions(Monad)

  @ocaml.doc("
  Converts the non-empty into a string delimited by the given string
  ")
  let mkString: (string, t<string>) => string = (delim, xs) =>
    switch xs {
    | NonEmpty(y, ys) => y ++ (delim ++ TailSequence.mkString(delim, ys))
    }

  @ocaml.doc("
  Reverses the NonEmpty
  ")
  let reverse: t<'a> => t<'a> = x =>
    switch x {
    | NonEmpty(head, tail) =>
      tail
      ->TailSequence.reverse
      ->TailSequence.uncons
      ->Relude_Option_Instances.map(
        ((tailReversedHead, tailReversedTail)) => NonEmpty(
          tailReversedHead,
          tailReversedTail->(TailSequence.append(head, _)),
        ),
        _,
      )
      ->(Relude_Option_Base.getOrElseLazy(() => pure(head), _))
    }

  @ocaml.doc("
  Indicates if two NonEmpty sequences are pair-wise equal
  ")
  let eqBy: 'a. (('a, 'a) => bool, t<'a>, t<'a>) => bool = (eqA, xs, ys) =>
    switch (xs, ys) {
    | (NonEmpty(x, xs), NonEmpty(y, ys)) => eqA(x, y) && TailSequence.eqBy(eqA, xs, ys)
    }

  @ocaml.doc("
  Indicates if two NonEmpty sequences are pair-wise equal using the given EQ module
  ")
  let eq = (type a, eqA: module(EQ with type t = a), xs: t<a>, ys: t<a>): bool => {
    module EqA = unpack(eqA)
    eqBy(EqA.eq, xs, ys)
  }

  module type EQ_F = (EqA: EQ) => (EQ with type t = t<EqA.t>)

  module Eq: EQ_F = (EqA: EQ) => {
    type t = t<EqA.t>
    let eq = (xs, ys) => eqBy(EqA.eq, xs, ys)
  }

  @ocaml.doc("
  Converts a NonEmpty to a string using the given show function
  ")
  let showBy: 'a. ('a => string, t<'a>) => string = (showX, xs) => {
    let strings = map(showX, xs)
    "[!" ++ (mkString(", ", strings) ++ "!]")
  }

  @ocaml.doc("
  Converts a NonEmpty to a string using the given SHOW module
  ")
  let show = (type a, showA: module(SHOW with type t = a), xs: t<a>): string => {
    module ShowA = unpack(showA)
    showBy(ShowA.show, xs)
  }

  module type SHOW_F = (S: SHOW) => (SHOW with type t = t<S.t>)

  module Show: SHOW_F = (S: SHOW) => {
    type t = t<S.t>
    let show = showBy(S.show, _)
  }

  @ocaml.doc("
  NonEmpty extensions when you have an APPLICATIVE instance
  ")
  module WithApplicative = (A: APPLICATIVE) => {
    module Traversable: TRAVERSABLE with type t<'a> = t<'a> and type applicative_t<'a> = A.t<'a> = {
      type t<'a> = t<'a>
      type applicative_t<'a> = A.t<'a>
      include (Functor: FUNCTOR with type t<'a> := t<'a>)
      include (Foldable: FOLDABLE with type t<'a> := t<'a>)
      module TailTraversable = TailSequence.Traversable(A)

      let traverse: 'a 'b. ('a => applicative_t<'b>, t<'a>) => applicative_t<t<'b>> = (
        f,
        NonEmpty(x, xs),
      ) => A.apply(A.map(x => make(x, ...), f(x)), TailTraversable.traverse(f, xs))

      let sequence: t<applicative_t<'a>> => applicative_t<t<'a>> = fa => traverse(x => x, fa)
    }
    let traverse = Traversable.traverse
    let sequence = Traversable.sequence
    include Relude_Extensions_Traversable.TraversableExtensions(Traversable)
  }
}

@ocaml.doc("
A NonEmpty implemented using a list as the tail sequence
")
module List = {
  include WithSequence(Relude_Sequence.List)
}

@ocaml.doc("
A NonEmpty implemented using an array as the tail sequence
")
module Array = {
  include WithSequence(Relude_Sequence.Array)

  let toNonEmptyList: 'a. t<'a> => List.t<'a> = (NonEmpty(h, tailArray)) => NonEmpty(
    h,
    Belt.List.fromArray(tailArray),
  )

  let fromNonEmptyList: 'a. List.t<'a> => t<'a> = (NonEmpty(h, tailList)) => NonEmpty(
    h,
    Belt.List.toArray(tailList),
  )
}
