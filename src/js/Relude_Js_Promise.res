@ocaml.doc(`
[Relude.Js.Promise] contains utilities for interoperating with [Js.Promise].
Many of these functions will help you convert to and from [Relude.IO].
`)
@ocaml.doc("
Lifts a [Js.Promise] into a [Relude.IO]

Note: prefer {!val:toIOLazy} over this function if possible. A [Js.Promise] is
eagerly executed, so using [toIO] with an already-constructed and running
[Js.Promise] will not suspend the side effects.
")
let toIO: 'a. promise<'a> => Relude_IO.t<'a, exn> = promise =>
  Relude_IO.async(onDone =>
    promise
    ->(Promise.then(v => Promise.resolve(onDone(Ok(v)))))
    ->(Promise.catch(e => Promise.resolve(onDone(Error(e)))))
    ->ignore
  )

@ocaml.doc("
Lifts a lazily-executed [Js.Promise] into a [Relude.IO].
")
let toIOLazy: 'a. (unit => promise<'a>) => Relude_IO.t<'a, exn> = runPromise =>
  Relude_IO.async(onDone => {
    let promise = runPromise()
    promise
    ->(Promise.then(v => Promise.resolve(onDone(Ok(v)))))
    ->(Promise.catch(e => Promise.resolve(onDone(Error(e)))))
    ->ignore
  })

// TODO: not sure how best to handle exn/Js.Exn.t/Js.Promise.error below...
// open to suggestions/ideas

@ocaml.doc("
Converts a [Relude.IO] into a [Js.Promise.t]. This function will cause the IO
effects to be run.

The promise that is returned will not reject, it will instead have a [result] as
its resolution.
")
let fromIOWithResult: 'a 'e. Relude_IO.t<'a, 'e> => promise<result<'a, 'e>> = io =>
  Promise.make((resolve, _) =>
    io->(Relude_IO.unsafeRunAsync(result => resolve(result), _))
  )
@ocaml.doc("
Converts a [Relude.IO] into a [Js.Promise.t]. This function will cause the IO
effects to be run.

The error channel is unsafely coerced into the promise error type, which is
probably fine, because the [Js.Promise] error type is opaque.
")
let fromIO: 'a 'e. Relude_IO.t<'a, 'e> => promise<'a> = io =>
  Promise.make((resolve, reject) =>
    io->(
      Relude_IO.unsafeRunAsync(
        result =>
          switch result {
          | Ok(v) => resolve(v)
          | Error(e) => reject(Relude_Unsafe.coerce(e))
          },
        /* TODO: not sure if this is wise/good */

        _,
      )
    )
  )

@ocaml.doc("
Converts a [Relude.IO] with an extensible OCaml [exn] as the error type into a
[Js.Promise.t]. This function will cause the IO effects to be run.
")
let fromIOExn: 'a. Relude_IO.t<'a, exn> => promise<'a> = io =>
  Promise.make((resolve, reject) => io->(Relude_IO.unsafeRunAsync(result =>
        switch result {
        | Ok(v) => resolve(v)
        | Error(e) => reject(e)
        }
      , _)))

@ocaml.doc("
Converts a [Relude.IO] with a [Js.Exn.t] as the error type into a
[Js.Promise.t]. This function will cause the IO effects to be run.
")
let fromIOJsExn: 'a. Relude_IO.t<'a, JsExn.t> => promise<'a> = io =>
  Promise.make((resolve, reject) => io->(Relude_IO.unsafeRunAsync(result =>
        switch result {
        | Ok(v) => resolve(v)
        | Error(e) => reject(Relude_Js_Exn.unsafeToExn(e))
        }
      , _)))
