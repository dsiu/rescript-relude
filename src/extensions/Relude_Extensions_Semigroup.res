@ocaml.doc("
Extensions for any SEMIGROUP
")
module SemigroupExtensions = (S: BsBastet.Interface.SEMIGROUP) => {
  let concatNamed = (~prefix: S.t, suffix: S.t) => S.append(prefix, suffix)
}

@ocaml.doc("
Infix operator extensions for any SEMIGROUP
")
module SemigroupInfix = (S: BsBastet.Interface.SEMIGROUP) => {
  let \"|+|" = S.append
}
