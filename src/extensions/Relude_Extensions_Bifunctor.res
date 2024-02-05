@@uncurried
@@uncurried.swap

@ocaml.doc("
Extensions for any Bifunctor
")
module BifunctorExtensions = (B: BsBastet.Interface.BIFUNCTOR) => {
  @ocaml.doc("
  Maps a function over the left-side type.
  ")
  let mapLeft: 'a 'b 'c. ('a => 'c, B.t<'a, 'b>) => B.t<'c, 'b> = (aToC, fab) =>
    B.bimap(aToC, b => b, fab)

  @ocaml.doc("
  Maps a function over the right-side type.
  ")
  let mapRight: 'a 'b 'd. ('b => 'd, B.t<'a, 'b>) => B.t<'a, 'd> = (bToD, fab) =>
    B.bimap(a => a, bToD, fab)

  @ocaml.doc("
  Alias for [mapRight]

  Note: this function only makes sense if your error type is on the right of
  the Bifunctor, like [result('a, 'e)], or [IO.t('a, 'e)].
  ")
  let mapError = mapRight
}

@ocaml.doc("
Infix operator extensions for any BIFUNCTOR
")
module BifunctorInfix = (B: BsBastet.Interface.BIFUNCTOR) => {
  @ocaml.doc("
  Operator version of bimap
  ")
  let \"<<$>>" = B.bimap
}
