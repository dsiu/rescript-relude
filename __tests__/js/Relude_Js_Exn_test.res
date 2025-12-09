open Jest
open Expect

module Exn = Relude_Js_Exn

describe("Js.Exn", () => {
  test("make", () => {
    let e = Exn.make("my error")
    expect(JsExn.message(e))->toEqual(Some("my error"))
  })

  test("throw", () => expect(() => Exn.throw("my error"))->toThrow)

  test("unsafeFromExn Js.Exn.Error", () =>
    switch JsError.throwWithMessage("my error") {
    | _ => fail("fail")
    | exception exn => expect(exn->Exn.unsafeFromExn->JsExn.message)->toEqual(Some("my error"))
    }
  )

  test("unsafeFromExn unknown", () => {
    let exn: exn = %raw(`"my error"`)
    expect(exn->Exn.unsafeFromExn->JsExn.message)->toEqual(Some("Unexpected error: my error"))
  })

  test("unsafeToExn", () => {
    Exn.make("my error")
    ->Exn.unsafeToExn
    ->Exn.unsafeFromExn
    ->JsExn.message
    ->expect
    ->toEqual(Some("Unexpected error: Error: my error"))
  })
})
