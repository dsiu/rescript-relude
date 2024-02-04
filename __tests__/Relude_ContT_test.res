@@uncurried
@@uncurried.swap

open Jest
open Expect

module FilePath = {
  type t = FilePath(string)
}

module Error = {
  type t = Error(string)

  module Type: BsBastet.Interface.TYPE with type t = t = {
    type t = t
  }
}

module Unit = {
  type t = unit

  module Type: BsBastet.Interface.TYPE with type t = t = {
    type t = t
  }
}

module Cont = Relude.Cont.WithResult(Unit.Type)

@ocaml.doc("
Callback-based API for reading a file (with a success)
")
let readFileSuccess = (
  FilePath(filePath): FilePath.t,
  onDone: result<string, Error.t> => unit,
): unit => onDone(Ok("Read file: " ++ filePath))

@ocaml.doc("
Callback-based API for reading a file (with an error)
")
let readFileError = (
  FilePath(filePath): FilePath.t,
  onDone: result<string, Error.t> => unit,
): unit => onDone(Error(Error("Failed to read file: " ++ filePath)))

@ocaml.doc("
Callback-based API for writing a file (with success)
")
let writeFileSuccess = (
  FilePath(_filePath): FilePath.t,
  _content: string,
  onDone: result<unit, Error.t> => unit,
): unit => onDone(Ok())

@ocaml.doc("
Callback-based API for writing a file (with an error)
")
let writeFileError = (
  FilePath(filePath): FilePath.t,
  _content: string,
  onDone: result<unit, Error.t> => unit,
): unit => onDone(Error(Error("Failed to write file: " ++ filePath)))

@ocaml.doc("
Continuation Monad for the callback-based API for reading a file (success)
")
let readFileSuccessCont: FilePath.t => Cont.t<result<string, Error.t>> = filePath =>
  Cont.make(readFileSuccess(filePath, _))

@ocaml.doc("
Continuation Monad for the callback-based API for reading a file (error)
")
let readFileError: FilePath.t => Cont.t<result<string, Error.t>> = filePath =>
  Cont.make(readFileError(filePath, _))

@ocaml.doc("
Continuation Monad for the callback-based API for writing a file (success)
")
let writeFileSuccessCont: (FilePath.t, string) => Cont.t<result<unit, Error.t>> = (
  filePath,
  content,
) => Cont.make(writeFileSuccess(filePath, content, _))

@ocaml.doc("
Continuation Monad for the callback-based API for writing a file (error)
")
let writeFileErrorCont: (FilePath.t, string) => Cont.t<result<unit, Error.t>> = (
  filePath,
  content,
) => Cont.make(writeFileError(filePath, content, _))

describe("ContT Identity", () => {
  open Cont.Infix

  testAsync("pure", onDone =>
    \"<$$>"(\"<$$>"(Cont.pure(42), value => expect(value)->toEqual(42)), onDone)->(
      Cont.runContT(() => (), _)
    )
  )

  testAsync("map (<$$>) success", onDone => {
    let filePath = FilePath.FilePath("test.txt")

    Cont.runContT(
      () => (),
      \"<$$>"(
        \"<$$>"(
          readFileSuccessCont(filePath),
          x =>
            switch x {
            | Ok(value) => expect(value)->toEqual("Read file: test.txt")
            | Error(_) => fail("Failed")
            },
        ),
        onDone,
      ),
    )
  })

  testAsync("bind (>>=) success", onDone => {
    let filePath1 = FilePath.FilePath("test1.txt")
    let filePath2 = FilePath.FilePath("test2.txt")
    let filePath3 = FilePath.FilePath("test3.txt")
    let expected = "Read file: test1.txt, Read file: test2.txt, Read file: test3.txt"

    Cont.runContT(
      () => (),
      \"<$$>"(
        \">>="(
          readFileSuccessCont(filePath1),
          x =>
            switch x {
            | Ok(content1) =>
              \">>="(
                readFileSuccessCont(filePath2),
                x =>
                  switch x {
                  | Ok(content2) =>
                    \"<$$>"(
                      readFileSuccessCont(filePath3),
                      x =>
                        switch x {
                        | Ok(content3) =>
                          let result = Relude.List.String.joinWith(
                            ", ",
                            list{content1, content2, content3},
                          )
                          expect(result)->toEqual(expected)
                        | Error(_) => fail("Failed 3")
                        },
                    )
                  | Error(_) => Cont.pure(fail("Failed 2"))
                  },
              )
            | Error(_) => Cont.pure(fail("Failed 1"))
            },
        ),
        onDone,
      ),
    )
  })
})

// Below is an extremely contrived example of using IO with ContT.  I'm not sure if it actually
// demonstrates anything useful, or is a correct/expected usage of ContT...

module ContT = Relude.ContT
module Result = Relude.Result
module IO = Relude.IO
module IOE = IO.WithError(Error.Type)
module ContIO = ContT.WithMonadAndResult(IOE.Monad, Unit.Type)

@ocaml.doc("
A callback-based API for converting a string to an int and running some IO effect
on the result.
")
let stringToIntCB: (
  string,
  Result.t<int, Error.t> => IO.t<unit, Error.t>,
) => IO.t<unit, Error.t> = (str, onDone) =>
  IO.flatMap(
    i => onDone(Ok(i)),
    Relude.Option.fold(
      IO.throw(Error.Error("Failed")),
      i => IO.pure(i),
      Relude.Int.fromString(str),
    ),
  )

@ocaml.doc("
A ContT version of the above ContT(IO) CB API
")
let stringToIntCont: string => ContIO.t<result<int, Error.t>> = str =>
  ContIO.make(stringToIntCB(str, _))

@ocaml.doc("
Simulates an effectful assertion function that compares two ints
")
let assertIntIO = (expected: int, actual: int, onDone: assertion => unit): IO.t<unit, Error.t> => {
  onDone(expect(actual)->toEqual(expected))
  IO.unit
}

@ocaml.doc("
Simulates an effectful assertion function that fails
")
let failIO = (onDone: assertion => unit): IO.t<unit, Error.t> => {
  onDone(fail("Failed"))
  IO.unit
}

describe("ContT IO", () => {
  open ContIO.Infix

  testAsync("map", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok() => ()
        | Error(_) => ()
        },
      ContIO.runContT(
        x =>
          switch x {
          | Ok(i) => assertIntIO(84, i, onDone)
          | Error(_) => failIO(onDone)
          },
        \"<$$>"(stringToIntCont("42"), res => Result.map(a => a * 2, res)),
      ),
    )
  )

  testAsync("Cont.map2", onDone =>
    IO.unsafeRunAsync(
      x =>
        switch x {
        | Ok() => ()
        | Error(_) => ()
        },
      ContIO.runContT(
        x =>
          switch x {
          | Ok(a) => assertIntIO(85, a, onDone)
          | Error(_) => failIO(onDone)
          },
        ContIO.map2(
          (res1, res2) =>
            switch (res1, res2) {
            | (Ok(a), Ok(b)) => Ok(a + b)
            | _ => Error(Error.Error("error"))
            },
          stringToIntCont("42"),
          stringToIntCont("43"),
        ),
      ),
    )
  )
})
