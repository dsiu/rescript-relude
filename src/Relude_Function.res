@@uncurried
@@uncurried.swap

open BsBastet.Interface

@@ocaml.text(`
[Relude.Function] contains many core functions like [identity], [flip],
[compose], [andThen], and some of the associated infix operators like [<<]
([compose]) and [>>] ([andThen]).

It also defines some typeclass instances like [Functor], [Apply], [Monad], etc.
for the ['r => 'a] function type.
`)

@ocaml.doc("
[identity(x)] returns [x]. This is useful when you need
to supply a function but don’t want to transform any values.
")
let identity: 'a. 'a => 'a = a => a

@ocaml.doc("
[id] is a synonym for [identity].
")
let id: 'a. 'a => 'a = identity

@ocaml.doc("
[const(x, y)] returns [x].

{[
  const(3, \"ignore\") == 3;
  const(\"keep\", -1) == \"keep\";
]}
")
let const: 'a 'b. ('a, 'b) => 'a = (a, _) => a

@ocaml.doc("
[flip(f, a, b)] has a two-parameter function [f()] as its
first parameter. It calls [f(b, a)], thus “flipping“ the
arguments to [f()].

{[
  let formula = (x, y) => {x + 2 * y};
  formula(3, 5) == 13;
  flip(formula, 5, 3) == 13;
]}
")
let flip: 'a 'b 'c. (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b)

@ocaml.doc("
[compose(f, g, a)] is the equivalent of [f(g(a))].


{[
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  compose(square, double, 3) == 36;
  compose(double, square, 3) == 18;
]}
")
let compose: 'a 'b 'c. (. 'b => 'c, 'a => 'b) => 'a => 'c = (f, g) => a => f(g(a))

@ocaml.doc("
[flipCompose(f, g, a)] is the equivalent of [g(f(a))].

{[
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  flipCompose(square, double, 3) == 18;
  flipCompose(double, square, 3) == 36;
]}
")
let flipCompose: 'a 'b 'c. ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, a) => g(f(a))

@ocaml.doc("
[andThen] is a synonym for [flipCompose]

You can use this synonym with the “pipe first“ operator:

{[
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  let addFive = (x) => {x + 5};

  let formula = square -> andThen(double) -> andThen(addFive);
  formula(3);
]}
")
let andThen: 'a 'b 'c. ('a => 'b, 'b => 'c, 'a) => 'c = flipCompose

@ocaml.doc("
Converts a function that takes a tuple 2 as an argument to a normal curried
function.
")
let curry2: 'a 'b 'c. ((('a, 'b)) => 'c, 'a, 'b) => 'c = (f, a, b) => f((a, b))

@ocaml.doc("
Converts a function that takes a tuple-3 as an argument to a normal curried
function.
")
let curry3: 'a 'b 'c 'd. ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd = (f, a, b, c) => f((a, b, c))

@ocaml.doc("
Converts a function that takes a tuple-4 as an argument to a normal curried
function.
")
let curry4: 'a 'b 'c 'd 'e. ((('a, 'b, 'c, 'd)) => 'e, 'a, 'b, 'c, 'd) => 'e = (f, a, b, c, d) =>
  f((a, b, c, d))

@ocaml.doc("
Converts a function that takes a tuple-5 as an argument to a normal curried
function.
")
let curry5: 'a 'b 'c 'd 'e 'f. ((('a, 'b, 'c, 'd, 'e)) => 'f, 'a, 'b, 'c, 'd, 'e) => 'f = (
  f,
  a,
  b,
  c,
  d,
  e,
) => f((a, b, c, d, e))

@ocaml.doc("
Converts a normal curried function of two arguments to a function that takes a
tuple-2 as an argument.
")
let uncurry2: 'a 'b 'c. (('a, 'b) => 'c, ('a, 'b)) => 'c = (f, (a, b)) => f(a, b)

@ocaml.doc("
Converts a normal curried function of 3 arguments to a function that takes a
tuple-3 as an argument.
")
let uncurry3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd = (f, (a, b, c)) => f(a, b, c)

@ocaml.doc("
Converts a normal curried function of 4 arguments to a function that takes a
tuple-4 as an argument.
")
let uncurry4: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd) => 'e, ('a, 'b, 'c, 'd)) => 'e = (
  f,
  (a, b, c, d),
) => f(a, b, c, d)

@ocaml.doc("
Converts a normal curried function of 5 arguments to a function that takes a
tuple-5 as an argument.
")
let uncurry5: 'a 'b 'c 'd 'e 'f. (('a, 'b, 'c, 'd, 'e) => 'f, ('a, 'b, 'c, 'd, 'e)) => 'f = (
  f,
  (a, b, c, d, e),
) => f(a, b, c, d, e)

@ocaml.doc("
[map] is a synonym for [compose] and is the equivalent of [f(g(a))].

{[
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  map(square, double, 3) == 36;
  map(double, square, 3) == 18;
]}
")
let map: 'a 'b 'r. (. 'a => 'b, 'r => 'a) => 'r => 'b = (aToB, rToA) => r =>
  aToB(rToA(r)) /* Same as compose */

//let apply_x = (rToAToB, rToA) => r => rToAToB(r)(rToA(r))
@ocaml.doc("
In [apply(hof, f, a)], [hof] is a higher-order function that takes one argument
and returns a new function that also takes one argument.

The result of [apply()] is equivalent to:

{[
  let g = hof(a);
  g(f(a))
]}

{[
  // This is the higher-order function
  let showResult = (n) => {
    (x: float) => {\"input \" ++ string_of_int(n)
      ++ \" yields \" ++ Js.Float.toString(x)
    }
  };

  let cube = (x) => { float_of_int(x * x * x) };

  apply(showResult, cube, 5) == \"input 5 yields 125\";
]}
")
let apply_x = (rToAToB, rToA) => r => rToAToB(r)(rToA(r))
let apply = (. rToAToB, rToA) => apply_x(rToAToB, rToA, ...)

//let apply: 'a 'b 'r. (('r, 'a) => 'b, 'r => 'a, 'r) => 'b = (rToAToB, rToA, r) =>
//  rToAToB(r, rToA(r))

@ocaml.doc("
[pure] is a synonym for [const]
")
let pure_x: 'a 'r. ('a, 'r) => 'a = (a, _) => a
let pure = a => pure_x(a, ...)
//let pure: 'a 'r. ('a, 'r) => 'a = (a, _) => a

@ocaml.doc("
In [bind(f, hof, a)], [hof] is a higher-order function that takes one argument
and returns a new function that also takes one argument.

  The result of [bind()] is equivalent to:

{[
  let g = hof(f(a));
  g(a)
]}

{[
  // This is the higher-order function
  let showResult = (x) => {
    (n: int) => {\"input \" ++ string_of_int(n)
      ++ \" yields \" ++ Js.Float.toString(x)
    }
  };

  let cube = (x) => { float_of_int(x * x * x) };

  bind(cube, showResult, 5) == \"input 5 yields 125\";
]}
")
let bind_x = (rToA, arToB, r) => arToB(rToA(r))(r)
//let bind: 'a 'b 'r. ('r => 'a, ('a, 'r) => 'b) => 'r => 'b = (rToA, arToB) =>
//  bind_x(rToA, arToB, ...)
let bind = (. rToA, arToB) => bind_x(rToA, arToB, ...)
//let bind: 'a 'b 'r. ('r => 'a, ('a, 'r) => 'b, 'r) => 'b = (rToA, arToB, r) => arToB(rToA(r), r)

@ocaml.doc("
In [flatMap(hof, f, a)], [hof] is a higher-order function that takes one
argument and returns a new function that also takes one argument. It is the same
as [bind], but with the first two arguments in reverse order.

{[
  // This is the higher-order function
  let showResult = (x) => {
    (n: int) => {\"input \" ++ string_of_int(n)
      ++ \" yields \" ++ Js.Float.toString(x)
    }
  };

  let cube = (x) => { float_of_int(x * x * x) };

  flatMap(showResult, cube, 5) == \"input 5 yields 125\";
]}
")
let flatMap: 'a 'b 'r. (
  ('a, 'r) => 'b,
  'r => 'a,
) => 'r => 'b = //  (f, fa) => r => bind(fa, x => f(x, _))(r)
(f, fa) => bind(fa, x => f(x, _))

@ocaml.doc("
[memoize0] takes a [unit => 'a] function and returns a new function
which once called, will store the result produced and return that cached
result for each successive call.
")
let memoize0 = (f: unit => 'a): (unit => 'a) => {
  let cached = ref(None)
  () =>
    switch cached.contents {
    | Some(result) => result
    | None =>
      let result = f()
      cached := Some(result)
      result
    }
}

@ocaml.doc("
[memoize1] takes a ['a => 'b] function and returns a new ['a => 'b] function
which once called, stores the result produced by the given function in
a closure-based cache, using a cache key created by the function [makeKey]

All successive calls to the function for input values that resolve to the same
cache key will return the cached result.
")
let memoize1 = (~makeKey: 'a => string, f: 'a => 'b): ('a => 'b) => {
  let cache = ref(Belt.Map.String.empty)
  input => {
    let key = makeKey(input)
    let resultOpt = Belt.Map.String.get(cache.contents, key)
    switch resultOpt {
    | Some(result) => result
    | None =>
      let result = f(input)
      cache := Belt.Map.String.set(cache.contents, key, result)
      result
    }
  }
}

@ocaml.doc("
Takes a function and returns a new function which when called, will allow the
first [times] calls to invoke the given function, and any successive calls will
be suppressed and the last result will be returned.
")
let before = (~times: int, f: unit => 'a): (unit => 'a) => {
  let callCount = ref(0)
  let lastResultOpt = ref(None)
  () =>
    switch lastResultOpt.contents {
    | Some(lastResult) =>
      if callCount.contents < times {
        let result = f()
        lastResultOpt := Some(result)
        callCount := callCount.contents + 1
        result
      } else {
        lastResult
      }
    | None =>
      let result = f()
      lastResultOpt := Some(result)
      callCount := callCount.contents + 1
      result
    }
}

@ocaml.doc("
Takes a function and returns a new function that when called, will suppress
the first [times] invocations.
")
let after = (~times: int, f: unit => 'a): (unit => option<'a>) => {
  let callCount = ref(0)
  () =>
    if callCount.contents < times {
      callCount := callCount.contents + 1
      None
    } else {
      Some(f())
    }
}

@ocaml.doc("
Takes a function and returns a new function which will invoke the given function
once, and any successive calls will be suppressed, returning the value of the
first call.
")
let once = (f: unit => 'a): (unit => 'a) => {
  let lastResultOpt = ref(None)
  () =>
    switch lastResultOpt.contents {
    | Some(lastResult) => lastResult
    | None =>
      let result = f()
      lastResultOpt := Some(result)
      result
    }
}

@ocaml.doc("
Takes a function from ['a => 'b] and a function from ['i => 'a] to modify the
input, and a function ['b => 'o] to modify the output, and returns a new
function ['i => 'o]
")
let wrap: //'i 'a 'b 'o. // TODO: not sure how to make this universal quantification work
(~before: 'i => 'a, ~after: 'b => 'o, 'a => 'b, 'i) => 'o = (
  ~before: 'i => 'a,
  ~after: 'b => 'o,
  f: 'a => 'b,
  input: 'i,
) => after(f(before(input)))

@ocaml.doc("
Takes a predicate function, and returns a new predicate function which negates
the given predicate.
")
let negate = (f: 'a => bool): ('a => bool) => a => !f(a)

//
// utils for converting curried functions to chained-uncurried function of single argument
// this is needed to simulate curried callbacks in ReScript v11 default uncurried mode
//
let uncurryFn2: 'a 'b 'r. ((. 'a, 'b) => 'r) => 'a => 'b => 'r = f => {
  a => b => f(a, b)
}

let uncurryFn3: 'a 'b 'c 'r. ((. 'a, 'b, 'c) => 'r) => 'a => 'b => 'c => 'r = f => {
  a => b => c => f(a, b, c)
}

let uncurryFn4: 'a 'b 'c 'd 'r. (('a, 'b, 'c, 'd) => 'r) => 'a => 'b => 'c => 'd => 'r = f => {
  a => b => c => d => f(a, b, c, d)
}

let uncurryFn5: 'a 'b 'c 'd 'e 'r. (
  ('a, 'b, 'c, 'd, 'e) => 'r
) => 'a => 'b => 'c => 'd => 'e => 'r = f => {
  a => b => c => d => e => f(a, b, c, d, e)
}

@ocaml.doc("
The [Infix] submodule provides two infix operators for function composition.

{[
  open Relude.Function.Infix;
]}
")
module Infix = {
  @ocaml.doc("
    The [<<] operator returns a function that is the equivalent of
    calling [compose()] with its function arguments.

    [(f << g)(x)] is the equivalent of [f(g(x))].


    {[
      let sqrtCompFloor = sqrt << floor;
      sqrtCompFloor(4.5) == 2.0;
    ]}
  ")
  let \"<<" = compose

  @ocaml.doc("
    The [>>] operator returns a function that is the equivalent of
    calling [flipCompose()] with its function arguments.

  [(f >> g)(x)] is the equivalent of [g(f(x))].


    {[
      let floorFlipSqrt = floor >> sqrt;
      floorFlipSqrt(4.5) == 2.0;
    ]}
  ")
  let \">>" = flipCompose
}

module WithArgument = (R: TYPE) => {
  module Functor: FUNCTOR with type t<'a> = R.t => 'a = {
    type t<'a> = R.t => 'a
    let map = map
  }
  let map = Functor.map
  include Relude_Extensions_Functor.FunctorExtensions(Functor)

  module Apply: APPLY with type t<'a> = R.t => 'a = {
    include Functor
    let apply = apply
  }
  let apply = Apply.apply
  include Relude_Extensions_Apply.ApplyExtensions(Apply)

  module Applicative: APPLICATIVE with type t<'a> = R.t => 'a = {
    include Apply
    let pure = pure
  }
  let pure = Applicative.pure
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative)

  module Monad: MONAD with type t<'a> = R.t => 'a = {
    include Applicative
    let flat_map = bind
  }
  let bind = Monad.flat_map
  include Relude_Extensions_Monad.MonadExtensions(Monad)

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor)
    include Relude_Extensions_Apply.ApplyInfix(Apply)
    include Relude_Extensions_Monad.MonadInfix(Monad)
    include Infix
  }
}
