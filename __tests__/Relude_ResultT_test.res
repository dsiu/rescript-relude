open Jest
open Expect

module IO = Relude.IO
module Result = Relude.Result
module ResultT = Relude.ResultT

type error = {message: string}

module Error = {
  type t = error
  module Type: BsBastet.Interface.TYPE with type t = t = {
    type t = t
  }
}

module IOE = IO.WithError(Error)

module ResultIOE = ResultT.WithMonadAndError(IOE.Monad, Error)

describe("ResultT", () => {
  testAsync("make", onDone =>
    ResultIOE.make(IOE.pure(Result.ok(42)))
    |> ResultIOE.map(a => expect(a) |> toEqual(42))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("withResultT/mapError", onDone =>
    ResultIOE.make(IO.pure(Error({message: "hi"})))
    |> ResultIOE.withResultT(e => {message: e.message ++ e.message})
    |> ResultIOE.mapError(e => expect(e.message) |> toEqual("hihi"))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Error(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("map", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.map(a => expect(a) |> toEqual(2))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("apply", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.apply(ResultIOE.pure(a => expect(a) |> toEqual(2)))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("pure", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.map(a => expect(a) |> toEqual(2))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("bind/flatMap", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.flatMap(a => ResultIOE.pure(expect(a) |> toEqual(2)))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("subflatMap", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.subflatMap(a => Result.pure(a + 3))
    |> ResultIOE.map(a => expect(a) |> toEqual(5))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("semiflatMap", onDone =>
    ResultIOE.pure(2)
    |> ResultIOE.semiflatMap(a => IO.pure(a + 3))
    |> ResultIOE.map(a => expect(a) |> toEqual(5))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("cond", onDone =>
    ResultIOE.pure(500)
    |> ResultIOE.cond(a => 9000 > a, 100, {message: "It's over 9000"})
    |> ResultIOE.map(a => expect(a) |> toEqual(100))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Ok(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )

  testAsync("condError", onDone =>
    ResultIOE.pure(10000)
    |> ResultIOE.condError(a => 9000 > a, {message: "It's over 9000"})
    |> ResultIOE.map(a => expect(a) |> toEqual(100))
    |> ResultIOE.mapError(e => expect(e.message) |> toEqual("It's over 9000"))
    |> ResultIOE.runResultT
    |> IO.unsafeRunAsync(x =>
      switch x {
      | Ok(Error(assertion)) => onDone(assertion)
      | _ => onDone(fail("fail"))
      }
    )
  )
})
