@ocaml.doc("
Extensions for any MONOID
")
module MonoidExtensions = (M: BsBastet.Interface.MONOID) => {
  module BsMonoidExtensions = BsBastet.Functions.Monoid(M)

  @ocaml.doc("
  Returns the monoidal value if the given condition is true, otherwise empty.
  ")
  let guard: (bool, M.t) => M.t = BsMonoidExtensions.guard

  @ocaml.doc("
  Combines the given monoidal value with itself the given number of times.
  ")
  let power: (M.t, int) => M.t = BsMonoidExtensions.power
}
