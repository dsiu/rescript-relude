open Jest
open Expect

module Exn = Relude_Js_Exn

describe("Js.Exn", () => {
  test("make", () => {
    let e = Exn.make("my error")
    expect(Js.Exn.message(e))->toEqual(Some("my error"))
  })

  test("throw", () => toThrow(expect(() => Exn.throw("my error"))))

  test("unsafeFromExn Js.Exn.Error", () =>
    switch Js.Exn.raiseError("my error") {
    | _ => fail("fail")
    | exception exn => expect(Js.Exn.message(Exn.unsafeFromExn(exn)))->toEqual(Some("my error"))
    }
  )

  test("unsafeFromExn unknown", () => {
    let exn: exn = %raw(`"my error"`)
    expect(Js.Exn.message(Exn.unsafeFromExn(exn)))->toEqual(Some("Unexpected error: my error"))
  })

  test("unsafeToExn", () =>
    expect(Js.Exn.message(Exn.unsafeFromExn(Exn.unsafeToExn(Exn.make("my error")))))->toEqual(
      Some("Unexpected error: Error: my error"),
    )
  )
})
