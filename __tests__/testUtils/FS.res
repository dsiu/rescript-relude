@ocaml.doc("
Fs.Native wraps a few functions from the node.js fs module with little to no
modification to the fs API.
")
module Native = {
  let dirname: option<string> = %raw(` typeof __dirname === "undefined" ? undefined : __dirname `)
  let dirnameOrDot = Option.getOr(dirname, ".")

  @val @module("fs") @warning("-103")
  external readFileSync: (string, [#hex | #utf8 | #ascii]) => string = "readFileSync"

  @val @module("fs")
  external writeFileSync: (string, string, [#hex | #utf8 | #ascii]) => unit = "writeFileSync"

  @val @module("fs")
  external readFile: (
    string,
    [#hex | #utf8 | #ascii],
    (Null.t<JsExn.t>, string) => unit,
  ) => unit = "readFile"

  @val @module("fs")
  external writeFile: (string, string, [#hex | #utf8 | #ascii], Null.t<JsExn.t> => unit) => unit =
    "writeFile"
}

@ocaml.doc("
Fs.IO wraps the synchronous Native functions in the IO monad.

Note: these fs functions can actually fail with exceptions with are not handled
by IO.
")
module IO = {
  // Read a file with no accomodation for errors
  let readFileSync: string => Relude_IO.t<string, JsExn.t> = path =>
    Relude_IO.triesJS(() => Native.readFileSync(path, #utf8))

  let writeFileSync: (string, string) => Relude_IO.t<unit, JsExn.t> = (path, content) =>
    Relude_IO.triesJS(() => Native.writeFileSync(path, content, #utf8))

  let readFile: string => Relude_IO.t<string, JsExn.t> = path =>
    Relude_IO.async(onDone =>
      Native.readFile(path, #utf8, (err, content) =>
        switch (Null.toOption(err), content) {
        | (Some(err'), _) =>
          Console.error(
            "Read failed: " ++
            JsExn.message(err')->(Relude_Option.getOrElseLazy(_ => "No error", _)),
          )
          onDone(Error(err'))
        | (_, content) => onDone(Ok(content))
        }
      )
    )

  let writeFile: (string, string) => Relude_IO.t<unit, JsExn.t> = (path, content) =>
    Relude_IO.async(onDone =>
      Native.writeFile(path, content, #utf8, err =>
        switch Null.toOption(err) {
        | Some(err') =>
          Console.error(
            "Write failed: " ++
            JsExn.message(err')->(Relude_Option.getOrElseLazy(_ => "No error", _)),
          )
          onDone(Error(err'))
        | None => onDone(Ok())
        }
      )
    )
}

let testFilePath: string => string = fileName => Native.dirnameOrDot ++ ("/" ++ fileName)
