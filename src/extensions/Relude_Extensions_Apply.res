@@uncurried
@@uncurried.swap

@ocaml.doc("
Extensions for any APPLY
")
module ApplyExtensions = (A: BsBastet.Interface.APPLY) => {
  module BsApplyExtensions = BsBastet.Functions.Apply(A)

  @ocaml.doc("
  Runs the two applicative effects, but only keeps the result from the left
  side.
  ")
  let applyFirst: 'a 'b. (A.t<'a>, A.t<'b>) => A.t<'a> = BsApplyExtensions.apply_first

  @ocaml.doc("
  Runs the two applicative effects, but only keeps the result from the right
  side.
  ")
  let applySecond: 'a 'b. (A.t<'a>, A.t<'b>) => A.t<'b> = BsApplyExtensions.apply_second

  @ocaml.doc("
  Runs the applicative effects and combines the results using the given pure
  function.
  ")
  let map2: 'a 'b 'c. (('a, 'b) => 'c, A.t<'a>, A.t<'b>) => A.t<'c> = BsApplyExtensions.lift2

  @ocaml.doc("
  Runs the applicative effects and combines the results using the given pure
  function.
  ")
  let map3: 'a 'b 'c 'd. (
    ('a, 'b, 'c) => 'd,
    A.t<'a>,
    A.t<'b>,
    A.t<'c>,
  ) => A.t<'d> = BsApplyExtensions.lift3

  @ocaml.doc("
  Runs the applicative effects and combines the results using the given pure
  function.
  ")
  let map4: 'a 'b 'c 'd 'e. (
    ('a, 'b, 'c, 'd) => 'e,
    A.t<'a>,
    A.t<'b>,
    A.t<'c>,
    A.t<'d>,
  ) => A.t<'e> = BsApplyExtensions.lift4

  @ocaml.doc("
  Runs the applicative effects and combines the results using the given pure
  function.
  ")
  let map5: 'a 'b 'c 'd 'e 'f. (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    A.t<'a>,
    A.t<'b>,
    A.t<'c>,
    A.t<'d>,
    A.t<'e>,
  ) => A.t<'f> = BsApplyExtensions.lift5

  @ocaml.doc("
  Runs the applicative effects and combines the results into a tuple.
  ")
  let tuple2: 'a 'b. (A.t<'a>, A.t<'b>) => A.t<('a, 'b)> = BsApplyExtensions.apply_both

  @ocaml.doc("
  Runs the applicative effects and combines the results into a tuple.
  ")
  let tuple3: 'a 'b 'c. (A.t<'a>, A.t<'b>, A.t<'c>) => A.t<('a, 'b, 'c)> = (fa, fb, fc) =>
    map3((a, b, c) => (a, b, c), fa, fb, fc)

  @ocaml.doc("
  Runs the applicative effects and combines the results into a tuple.
  ")
  let tuple4: 'a 'b 'c 'd. (A.t<'a>, A.t<'b>, A.t<'c>, A.t<'d>) => A.t<('a, 'b, 'c, 'd)> = (
    fa,
    fb,
    fc,
    fd,
  ) => map4((a, b, c, d) => (a, b, c, d), fa, fb, fc, fd)

  @ocaml.doc("
  Runs the applicative effects and combines the results into a tuple.
  ")
  let tuple5: 'a 'b 'c 'd 'e. (
    A.t<'a>,
    A.t<'b>,
    A.t<'c>,
    A.t<'d>,
    A.t<'e>,
  ) => A.t<('a, 'b, 'c, 'd, 'e)> = (fa, fb, fc, fd, fe) =>
    map5((a, b, c, d, e) => (a, b, c, d, e), fa, fb, fc, fd, fe)

  @ocaml.doc("
  Runs the applicative effects in the tuple, and combines the results using the
  given pure function.
  ")
  let mapTuple2: 'a 'b 'c. (('a, 'b) => 'c, (A.t<'a>, A.t<'b>)) => A.t<'c> = (f, (fa, fb)) =>
    map2(f, fa, fb)

  @ocaml.doc("
  Runs the applicative effects in the tuple, and combines the results using the
  given pure function.
  ")
  let mapTuple3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, (A.t<'a>, A.t<'b>, A.t<'c>)) => A.t<'d> = (
    f,
    (fa, fb, fc),
  ) => map3(f, fa, fb, fc)

  @ocaml.doc("
  Runs the applicative effects in the tuple, and combines the results using the
  given pure function.
  ")
  let mapTuple4: 'a 'b 'c 'd 'e. (
    ('a, 'b, 'c, 'd) => 'e,
    (A.t<'a>, A.t<'b>, A.t<'c>, A.t<'d>),
  ) => A.t<'e> = (f, (fa, fb, fc, fd)) => map4(f, fa, fb, fc, fd)

  @ocaml.doc("
  Runs the applicative effects in the tuple, and combines the results using the
  given pure function.
  ")
  let mapTuple5: 'a 'b 'c 'd 'e 'f. (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    (A.t<'a>, A.t<'b>, A.t<'c>, A.t<'d>, A.t<'e>),
  ) => A.t<'f> = (f, (fa, fb, fc, fd, fe)) => map5(f, fa, fb, fc, fd, fe)
}

@ocaml.doc("
Infix operator extensions for any APPLY
")
module ApplyInfix = (A: BsBastet.Interface.APPLY) => {
  module ApplyExtensions = ApplyExtensions(A)

  @ocaml.doc("
  Applies the applicative function on the left to the applicative value on the
  right.
  ")
  let \"<*>" = A.apply

  @ocaml.doc("
  Runs the two applicative effects, and returns the result from the left side.
  ")
  let \"<*" = ApplyExtensions.applyFirst

  @ocaml.doc("
  Runs the two applicative effects, and returns the result from the right side.
  ")
  let \"*>" = ApplyExtensions.applySecond
}
