@@uncurried
@@uncurried.swap

open BsBastet.Interface

module WithFunctor = (F: FUNCTOR) => {
  type rec t<'a> =
    | Pure('a)
    | FlatMap(F.t<t<'a>>)

  let rec map: 'a 'b. ('a => 'b, t<'a>) => t<'b> = (aToB, freeA) =>
    switch freeA {
    | Pure(a) => Pure(aToB(a))
    | FlatMap(fFreeA) => FlatMap(fFreeA->(F.map(freeA => freeA->(map(aToB, _)), _)))
    }

  module Functor: FUNCTOR with type t<'a> = t<'a> = {
    type t<'a> = t<'a>
    let map = map
  }
  include Relude_Extensions_Functor.FunctorExtensions(Functor)

  let rec apply: 'a 'b. (t<'a => 'b>, t<'a>) => t<'b> = (freeAToB, freeA) =>
    switch freeAToB {
    | Pure(aToB) => freeA->(map(aToB, _))
    | FlatMap(fFreeAToB) => FlatMap(fFreeAToB->(F.map(freeAToB => apply(freeAToB, freeA), _)))
    }

  module Apply: APPLY with type t<'a> = t<'a> = {
    include Functor
    let apply = apply
  }
  include Relude_Extensions_Apply.ApplyExtensions(Apply)

  let pure: 'a. 'a => t<'a> = a => Pure(a)

  module Applicative: APPLICATIVE with type t<'a> = t<'a> = {
    include Apply
    let pure = pure
  }
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative)

  let rec bind: 'a 'b. (t<'a>, 'a => t<'b>) => t<'b> = (freeA, aToFreeB) =>
    switch freeA {
    | Pure(a) => aToFreeB(a)
    | FlatMap(fFreeA) => FlatMap(fFreeA->(F.map(freeA => bind(freeA, aToFreeB), _)))
    }

  module Monad: MONAD with type t<'a> = t<'a> = {
    include Applicative
    let flat_map = bind
  }
  include Relude_Extensions_Monad.MonadExtensions(Monad)

  @ocaml.doc("
  Lifts a value of our algebra into our free monad type

  This is useful to creating \"smart constructors\" for our algebra
  ")
  let liftF: 'a. F.t<'a> => t<'a> = fa => FlatMap(fa->(F.map(a => Pure(a), _)))

  @ocaml.doc("
  Specifies a monad into which we will interpret our free monadic program
  ")
  module WithMonad = (M: MONAD) => {
    @ocaml.doc("
    Applies an interpreter function to interpret each value of our algebra into
    a target monad.
    ")
    let rec foldFree: (F.t<'x> => M.t<'x>, t<'a>) => M.t<'a> = (nat, freeA) =>
      switch freeA {
      | Pure(a) => M.pure(a)
      | FlatMap(fFreeA) => M.flat_map(nat(fFreeA), foldFree(nat, ...))
      }
  }

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor)
    include Relude_Extensions_Apply.ApplyInfix(Apply)
    include Relude_Extensions_Monad.MonadInfix(Monad)
  }
}
