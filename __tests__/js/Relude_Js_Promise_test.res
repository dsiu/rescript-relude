open Jest
open Expect

module IO = Relude_IO
module RPromise = Relude_Js_Promise

describe("Js.Promise", () => {
  testAsync("toIO success", onDone =>
    Promise.resolve(42)
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
    Promise.reject(Relude_Unsafe.coerce("my error toIO"))
    ->Relude_Js_Promise.toIO
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("failed"))
          | Error(JsExn(e)) =>
            let str: string = Relude_Unsafe.coerce(e)
            onDone(expect(str)->toEqual("my error toIO"))
          | Error(_) => onDone(fail("failed"))
          },
        _,
      )
    )
  )

  testAsync("toIOLazy success", onDone =>
    (() => Promise.resolve(42))
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
    (() => Promise.reject(Relude_Unsafe.coerce("my error toIOLazy")))
    ->Relude_Js_Promise.toIOLazy
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("failed"))
          | Error(JsExn(e)) =>
            let str: string = Relude_Unsafe.coerce(e)
            onDone(expect(str)->toEqual("my error toIOLazy"))
          | Error(_) => onDone(fail("failed"))
          },
        _,
      )
    )
  )

  testPromise("fromIOWithResult success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOWithResult
    ->Promise.then(actual => actual->expect->toEqual(Ok(42))->Promise.resolve)
  )

  testPromise("fromIOWithResult error", () =>
    Relude_IO.throw(42)
    ->Relude_Js_Promise.fromIOWithResult
    ->Promise.then(actual => actual->expect->toEqual(Error(42))->Promise.resolve)
  )

  testPromise("fromIO success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIO
    ->Promise.then(actual => actual->expect->toEqual(42)->Promise.resolve)
  )

  testPromise("fromIO error", () =>
    Relude_IO.throw(42)
    ->Relude_Js_Promise.fromIO
    ->(Promise.then(_ => fail("fail")->Promise.resolve))
    ->(
      Promise.catch(
        error =>
          switch error {
          | JsExn(e) => e->Relude_Unsafe.coerce->expect->toEqual(42)->Promise.resolve
          | _ => fail("Unexpected error occurred")->Promise.resolve
          }
      )
    )
  )

  testPromise("fromIOExn success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOExn
    ->(Promise.then(actual => actual->expect->toEqual(42)->Promise.resolve))
  )

  testPromise("fromIOExn error", () =>
    Relude_IO.suspendThrow(() => Relude_Js_Exn.make("exn")->Relude_Js_Exn.unsafeToExn)
    ->Relude_Js_Promise.fromIOExn
    ->(Promise.then(_ => fail("fail")->Promise.resolve))
    ->(
      Promise.catch(
        error =>
          switch error {
          | JsExn(e) =>
              e
              ->Relude_Unsafe.coerce
              ->expect
              ->toEqual(Relude_Js_Exn.make("exn")->Relude_Js_Exn.unsafeToExn)
              ->Promise.resolve
          | _ => fail("Unexpected error occurred")->Promise.resolve
          }
      )
    )
  )

  testPromise("fromIOJsExn success", () =>
    Relude_IO.pure(42)
    ->Relude_Js_Promise.fromIOJsExn
    ->(Promise.then(actual => actual->expect->toEqual(42)->Promise.resolve))
  )

  testPromise("fromIOJsExn error", () =>
    Relude_IO.suspendThrow(() => Relude_Js_Exn.make("js_exn"))
    ->Relude_Js_Promise.fromIOJsExn
    ->(Promise.then(_ => fail("fail")->Promise.resolve))
    ->(
      Promise.catch(
        error =>
          switch error {
          | JsExn(e) =>
              e
              ->Relude_Unsafe.coerce
              ->expect
              ->toEqual(Relude_Js_Exn.make("js_exn"))
              ->Promise.resolve
          | _ => fail("Unexpected error occurred")->Promise.resolve
          }
      )
    )
  )
})
