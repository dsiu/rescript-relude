@@uncurried
@@uncurried.swap

open Jest
open Expect

module IO = Relude_IO
module RPromise = Relude_Js_Promise

describe("Js.Promise", () => {
  testAsync("toIO success", onDone =>
    Js.Promise.resolve(42)
    ->Relude_Js_Promise.toIO
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("failed"))
          },
        _,
      )
    )
  )

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIO error", onDone =>
    Js.Promise.reject(Relude_Unsafe.coerce("my error toIO"))
    ->Relude_Js_Promise.toIO
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("failed"))
          | Error(e) =>
            let str: string = Relude_Unsafe.coerce(e)
            onDone(expect(str)->toEqual("my error toIO"))
          },
        _,
      )
    )
  )

  testAsync("toIOLazy success", onDone =>
    (() => Js.Promise.resolve(42))
    ->Relude_Js_Promise.toIOLazy
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("failed"))
          },
        _,
      )
    )
  )

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIOLazy error", onDone =>
    (() => Js.Promise.reject(Relude_Unsafe.coerce("my error toIOLazy")))
    ->Relude_Js_Promise.toIOLazy
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("failed"))
          | Error(e) =>
            let str: string = Relude_Unsafe.coerce(e)
            onDone(expect(str)->toEqual("my error toIOLazy"))
          },
        _,
      )
    )
  )

  testPromise("fromIOWithResult success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOWithResult
    ->(Js.Promise.then_(actual => actual->expect->toEqual(Ok(42))->Js.Promise.resolve, _))
  )

  testPromise("fromIOWithResult error", () =>
    Relude_IO.throw(42)
    ->Relude_Js_Promise.fromIOWithResult
    ->(Js.Promise.then_(actual => actual->expect->toEqual(Error(42))->Js.Promise.resolve, _))
  )

  testPromise("fromIO success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIO
    ->(Js.Promise.then_(actual => actual->expect->toEqual(42)->Js.Promise.resolve, _))
  )

  testPromise("fromIO error", () =>
    Relude_IO.throw(42)
    ->Relude_Js_Promise.fromIO
    ->Js.Promise.then_(_ => fail("fail")->Js.Promise.resolve, _)
    ->(
      Js.Promise.catch(
        error => error->Relude_Unsafe.coerce->expect->toEqual(42)->Js.Promise.resolve,
        _,
      )
    )
  )

  testPromise("fromIOExn success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOExn
    ->(Js.Promise.then_(actual => actual->expect->toEqual(42)->Js.Promise.resolve, _))
  )

  testPromise("fromIOExn error", () =>
    Relude_IO.suspendThrow(() => Relude_Js_Exn.make("exn")->Relude_Js_Exn.unsafeToExn)
    ->Relude_Js_Promise.fromIOExn
    ->Js.Promise.then_(_ => fail("fail")->Js.Promise.resolve, _)
    ->(
      Js.Promise.catch(
        error =>
          error
          ->Relude_Unsafe.coerce
          ->expect
          ->toEqual(Relude_Js_Exn.make("exn")->Relude_Js_Exn.unsafeToExn)
          ->Js.Promise.resolve,
        _,
      )
    )
  )

  testPromise("fromIOJsExn success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOJsExn
    ->(Js.Promise.then_(actual => actual->expect->toEqual(42)->Js.Promise.resolve, _))
  )

  testPromise("fromIOJsExn error", () =>
    Relude_IO.suspendThrow(() => Relude_Js_Exn.make("js_exn"))
    ->Relude_Js_Promise.fromIOJsExn
    ->Js.Promise.then_(_ => fail("fail")->Js.Promise.resolve, _)
    ->(
      Js.Promise.catch(
        error =>
          error
          ->Relude_Unsafe.coerce
          ->expect
          ->toEqual(Relude_Js_Exn.make("js_exn"))
          ->Js.Promise.resolve,
        _,
      )
    )
  )
})
