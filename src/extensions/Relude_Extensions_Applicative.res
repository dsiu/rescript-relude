@ocaml.doc("
Extensions for any APPLICATIVE
")
module ApplicativeExtensions = (A: BsBastet.Interface.APPLICATIVE) => {
  module BsApplicativeExtensions = BsBastet.Functions.Applicative(A)

  @ocaml.doc("
  Lifts a pure function ['a => 'b] into an applicative context
  [A.t('a) => A.t('b)]
  ")
  let liftA1: 'a 'b. ('a => 'b, A.t<'a>) => A.t<'b> = BsApplicativeExtensions.liftA1

  @ocaml.doc("
  Runs the given applicative effect when the condition is true. If the condition
  is false, the effect is not run.
  ")
  let when_: (bool, A.t<unit>) => A.t<unit> = BsApplicativeExtensions.when_

  @ocaml.doc("
  Runs the given applicative effect when the condition is false. If the
  condition is true, the effect is not run.
  ")
  let unless: (bool, A.t<unit>) => A.t<unit> = BsApplicativeExtensions.unless

  let rec all: 'a. list<A.t<'a>> => A.t<list<'a>> = x =>
    switch x {
    | list{x, ...xs} =>
      xs->all->(A.apply(x->(A.map(result => rest => list{result, ...rest}, _)), _))
    | list{} => A.pure(list{})
    }
}
