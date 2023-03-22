@ocaml.doc("
Extensions for any MONOID_ANY
")
module MonoidAnyExtensions = (M: BsBastet.Interface.MONOID_ANY) => {
  @ocaml.doc("
  Returns the monoidal value if the given condition is true, otherwise empty.
  ")
  let guard: (bool, M.t<'a>) => M.t<'a> = (condition, ma) =>
    if condition {
      ma
    } else {
      M.empty
    }

  @ocaml.doc("
  Combines the given monoidal value with itself the given number of times.
  ")
  let power: (M.t<'a>, int) => M.t<'a> = (ma, times) => {
    let rec go = count =>
      switch count {
      | count if count <= 0 => M.empty
      | count if count == 1 => ma
      | count if mod(count, 2) == 0 =>
        let ma' = go(count / 2)
        M.append(ma', ma')
      | _ =>
        let ma' = go(count / 2)
        M.append(M.append(ma', ma'), ma)
      }
    go(times)
  }
}
