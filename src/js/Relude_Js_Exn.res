@ocaml.doc("
Creates a JS Error with the given string message.
")
let make: string => JsExn.t = (message) => JsError.make(message)->JsError.toJsExn

@ocaml.doc("
Creates and throw a JS Error with the given string message.
")
let throw: string => unit = (message) => JsError.make(message)->JsError.throw

@ocaml.doc("
Unsafely Converts an OCaml exn into a Js.Exn.t
")
let unsafeFromExn: exn => JsExn.t = exn => {
  let makeUnknownJsExn: exn => JsExn.t = %raw(` function(exn) { return new Error("Unexpected error: " + exn); } `)
  switch exn {
  | JsExn(jsExn) => jsExn
  | _ => makeUnknownJsExn(exn)
  }
}

@ocaml.doc("
Unsafely coerces a Js.Exn.t to an OCaml exn without regard to consequences of such actions.
")
external unsafeToExn: JsExn.t => exn = "%identity"
