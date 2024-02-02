open Jest
open Expect

module IO = Relude_IO
module RPromise = Relude_Js_Promise

describe("Js.Promise", () => {
  testAsync("toIO success", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok(value) => onDone(expect(value)->toEqual(42))
        | Error(_) => onDone(fail("failed"))
        },
      Relude_Js_Promise.toIO(Js.Promise.resolve(42)),
    )
  )

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIO error", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok(_) => onDone(fail("failed"))
        | Error(e) =>
          let str: string = Relude_Unsafe.coerce(e)
          onDone(expect(str)->toEqual("my error toIO"))
        },
      Relude_Js_Promise.toIO(Js.Promise.reject(Relude_Unsafe.coerce("my error toIO"))),
    )
  )

  testAsync("toIOLazy success", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok(value) => onDone(expect(value)->toEqual(42))
        | Error(_) => onDone(fail("failed"))
        },
      Relude_Js_Promise.toIOLazy(() => Js.Promise.resolve(42)),
    )
  )

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIOLazy error", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok(_) => onDone(fail("failed"))
        | Error(e) =>
          let str: string = Relude_Unsafe.coerce(e)
          onDone(expect(str)->toEqual("my error toIOLazy"))
        },
      Relude_Js_Promise.toIOLazy(
        () => Js.Promise.reject(Relude_Unsafe.coerce("my error toIOLazy")),
      ),
    )
  )

  testPromise("fromIOWithResult success", () =>
    Js.Promise.then_(
      actual => Js.Promise.resolve(expect(actual)->toEqual(Ok(42))),
      Relude_Js_Promise.fromIOWithResult(Relude_IO.pure(42)),
    )
  )

  testPromise("fromIOWithResult error", () =>
    Js.Promise.then_(
      actual => Js.Promise.resolve(expect(actual)->toEqual(Error(42))),
      Relude_Js_Promise.fromIOWithResult(Relude_IO.throw(42)),
    )
  )

  testPromise("fromIO success", () =>
    Js.Promise.then_(
      actual => Js.Promise.resolve(expect(actual)->toEqual(42)),
      Relude_Js_Promise.fromIO(Relude_IO.pure(42)),
    )
  )

  testPromise("fromIO error", () =>
    Js.Promise.catch(
      error => Js.Promise.resolve(expect(Relude_Unsafe.coerce(error))->toEqual(42)),
      Js.Promise.then_(
        _ => Js.Promise.resolve(fail("fail")),
        Relude_Js_Promise.fromIO(Relude_IO.throw(42)),
      ),
    )
  )

  testPromise("fromIOExn success", () =>
    Js.Promise.then_(
      actual => Js.Promise.resolve(expect(actual)->toEqual(42)),
      Relude_Js_Promise.fromIOExn(Relude_IO.pure(42)),
    )
  )

  testPromise("fromIOExn error", () =>
    Js.Promise.catch(
      error =>
        Js.Promise.resolve(
          expect(Relude_Unsafe.coerce(error))->toEqual(
            Relude_Js_Exn.unsafeToExn(Relude_Js_Exn.make("exn")),
          ),
        ),
      Js.Promise.then_(
        _ => Js.Promise.resolve(fail("fail")),
        Relude_Js_Promise.fromIOExn(
          Relude_IO.suspendThrow(() => Relude_Js_Exn.unsafeToExn(Relude_Js_Exn.make("exn"))),
        ),
      ),
    )
  )

  testPromise("fromIOJsExn success", () =>
    Js.Promise.then_(
      actual => Js.Promise.resolve(expect(actual)->toEqual(42)),
      Relude_Js_Promise.fromIOJsExn(Relude_IO.pure(42)),
    )
  )

  testPromise("fromIOJsExn error", () =>
    Js.Promise.catch(
      error =>
        Js.Promise.resolve(
          expect(Relude_Unsafe.coerce(error))->toEqual(Relude_Js_Exn.make("js_exn")),
        ),
      Js.Promise.then_(
        _ => Js.Promise.resolve(fail("fail")),
        Relude_Js_Promise.fromIOJsExn(Relude_IO.suspendThrow(() => Relude_Js_Exn.make("js_exn"))),
      ),
    )
  )
})
