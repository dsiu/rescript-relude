@@uncurried
@@uncurried.swap

open Jest
open Expect
open! Relude.Globals

let throwJSError: unit => int = %raw(` function() { throw new Error("Error from JS"); } `)

Jest.useFakeTimers() // This applies to the whole file, so any tests that use delay must use the mock timer manipulation functions

describe("IO basics", () => {
  testAsync("pure unsafeRunAsync", onDone =>
    IO.pure(42)->(
      IO.unsafeRunAsync(
        x => {
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          }
        },
        _,
      )
    )
  )

  testAsync("pureWithVoid unsafeRunAsync", onDone =>
    IO.pureWithVoid(42)->(
      IO.unsafeRunAsync(
        x => {
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          }
        },
        _,
      )
    )
  )

  testAsync("throw unsafeRunAsync", onDone =>
    IO.throwWithVoid("this is a test")->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_a) => onDone(fail("Failed"))
          | Error(_err) => onDone(pass)
          },
        _,
      )
    )
  )

  testAsync("suspend unsafeRunAsync", onDone =>
    IO.suspend(() => 42)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspendWithVoid unsafeRunAsync", onDone =>
    IO.suspendWithVoid(() => 42)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspendIO pure unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(42))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspendIO suspend unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.suspend(() => 42))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspendIO suspendIO pure unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.suspendIO(() => IO.pure(42)))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspendIO async unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.async(onDone => onDone(Ok(42))))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("async Ok unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Ok(42)))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("async Error unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Error("it failed")))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(msg) => onDone(expect(msg)->toEqual("it failed"))
          },
        _,
      )
    )
  )

  testAsync("pure map unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.map(a => a + 10, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(52))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("pure map <$$> unsafeRunAsync", onDone => {
    let \"<$$>" = IO.\"<$$>"

    \"<$$>"(IO.pure(42), a => a + 10)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(52))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  })

  testAsync("pure tap unsafeRunAsync", onDone => {
    let a = ref(0)

    IO.pure(42)
    ->IO.tap(b => a := b, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_value) => onDone(expect(a.contents)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  })

  testAsync("pure tapError unsafeRunAsync", onDone => {
    let a = ref(0)

    IO.throw(42)
    ->IO.tapError(b => a := b, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(_) => onDone(expect(a.contents)->toEqual(42))
          },
        _,
      )
    )
  })

  testAsync("pure apply unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.apply(IO.pure(a => a * 2), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(84))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("align pure pure", onDone =>
    IO.align(IO.pure(42), IO.pure("a"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(ior) => onDone(expect(ior)->toEqual(Relude_Ior_Type.Both(42, "a")))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )

  testAsync("align pure throw", onDone =>
    IO.align(IO.pure(42), IO.throw("e2"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(ior) => onDone(expect(ior)->toEqual(Relude_Ior_Type.This(42)))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )

  testAsync("align throw pure", onDone =>
    IO.align(IO.throw("e1"), IO.pure(99))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(ior) => onDone(expect(ior)->toEqual(Relude_Ior_Type.That(99)))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )

  testAsync("align throw throw", onDone =>
    IO.align(IO.throw("e1"), IO.throw("e2"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Fail"))
          | Error(e) => onDone(expect(e)->toEqual("e1"))
          },
        _,
      )
    )
  )

  testAsync("alignWith pure pure", onDone => {
    let f = x =>
      switch x {
      | Relude_Ior_Type.This(a) => a
      | Relude_Ior_Type.That(b) => int_of_string(b)
      | Relude_Ior_Type.Both(a, b) => a + int_of_string(b)
      }
    IO.alignWith(f, IO.pure(42), IO.pure("99"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(v) => onDone(expect(v)->toEqual(141))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  })

  testAsync("alignWith pure throw", onDone => {
    let f = x =>
      switch x {
      | Relude_Ior_Type.This(a) => a
      | Relude_Ior_Type.That(b) => int_of_string(b)
      | Relude_Ior_Type.Both(a, b) => a + int_of_string(b)
      }
    IO.alignWith(f, IO.pure(42), IO.throw("e2"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(v) => onDone(expect(v)->toEqual(42))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  })

  testAsync("alignWith throw pure", onDone => {
    let f = x =>
      switch x {
      | Relude_Ior_Type.This(a) => a
      | Relude_Ior_Type.That(b) => int_of_string(b)
      | Relude_Ior_Type.Both(a, b) => a + int_of_string(b)
      }
    IO.alignWith(f, IO.throw("e1"), IO.pure("99"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(v) => onDone(expect(v)->toEqual(99))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  })

  testAsync("alignWith throw throw", onDone => {
    let f = x =>
      switch x {
      | Relude_Ior_Type.This(a) => a
      | Relude_Ior_Type.That(b) => int_of_string(b)
      | Relude_Ior_Type.Both(a, b) => a + int_of_string(b)
      }
    IO.alignWith(f, IO.throw("e1"), IO.throw("e2"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Fail"))
          | Error(e) => onDone(expect(e)->toEqual("e1"))
          },
        _,
      )
    )
  })

  testAsync("pure flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flatMap(a => IO.pure(a + 10), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(52))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("throw flatMap unsafeRunAsync", onDone =>
    IO.throw(42)
    ->IO.flatMap(a => IO.pure(a + 1), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(value) => onDone(expect(value)->toEqual(42))
          },
        _,
      )
    )
  )

  testAsync("pure flatMap pure flatMap pure flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flatMap(a => IO.pure(a + 10), _)
    ->IO.flatMap(a => IO.pure(a + 10), _)
    ->IO.flatMap(a => IO.pure(a + 10), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(72))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("pure flatMap suspend flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flatMap(a => IO.suspend(_ => a + 10)->(IO.flatMap(b => IO.pure(b * 2), _)), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(value) => onDone(expect(value)->toEqual(104))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )
})

describe("IO fromOption", () => {
  testAsync("fromOption Some unsafeRunAsync", onDone =>
    IO.fromOption(() => "Failed", Some(32))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(32))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("fromOption None unsafeRunAsync", onDone =>
    IO.fromOption(() => "Messed up", None)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(error) => onDone(expect(error)->toEqual("Messed up"))
          },
        _,
      )
    )
  )
})

describe("IO fromResult", () => {
  testAsync("fromResult Ok unsafeRunAsync", onDone =>
    IO.fromResult(Ok(32))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(32))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("fromResult Error unsafeRunAsync", onDone =>
    IO.fromResult(Error("Messed up"))->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(error) => onDone(expect(error)->toEqual("Messed up"))
          },
        _,
      )
    )
  )
})

describe("IO cond", () => {
  testAsync("pure cond map unsafeRunAsync", onDone =>
    IO.pure("hello")
    ->IO.cond(a => a->String.length == 5, "is five", "boom explosions", _)
    ->IO.map(a => expect(a)->toEqual("is five"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | _ => onDone(fail("fail"))
          },
        _,
      )
    )
  )

  testAsync("throw cond error unsafeRunAsync", onDone =>
    IO.pure("hel")
    ->IO.cond(a => a->String.length == 5, "is five", "boom explosions", _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("fail"))
          | Error(a) => onDone(expect(a)->toEqual("boom explosions"))
          },
        _,
      )
    )
  )

  testAsync("pure condError mapError unsafeRunAsync", onDone =>
    IO.pure("hello world")
    ->IO.condError(a => a->String.length == 5, "string is too long", _)
    ->IO.mapError(a => expect(a)->toEqual("string is too long"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("fail"))
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("pure condError ok unsafeRunAsync", onDone =>
    IO.pure("hello")
    ->IO.condError(a => a->String.length == 5, "string is too long", _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual("hello"))
          | Error(_) => onDone(fail("fail"))
          },
        _,
      )
    )
  )
})

describe("IO compose", () => {
  describe("pure", () => {
    let ioAToB = IO.pure(a => a ++ "1")

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        {
          open IO
          \"<<<"(ioBToC, ioAToB)
        }->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "pure (andThen)",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        {
          open IO
          \">>>"(ioAToB, ioBToC)
        }->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Result.ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Result.error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))
        let ioAToB = IO.pure(a => a ++ "1")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a("0"))->toEqual("012"))
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("throw", () => {
    let ioAToB = IO.throw("error")

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(i => i + 42)

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error 2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => i => i + 42)

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(i => i + 42))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Result.ok(i => i + 42)))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Result.error("error 2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(a => b => a + b, IO.pure(1))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(a => b => a + b), IO.pure(1))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(a => IO.pure(b => a + b), IO.pure(0))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => onDone(expect(e)->toEqual("error"))
              },
            _,
          )
        )
      },
    )
  })

  describe("suspend", () => {
    let ioAToB = IO.suspend(() => a => a ++ "1")

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("suspendIO", () => {
    let ioAToB = IO.suspendIO(() => IO.pure(a => a ++ "1"))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("async (ok)", () => {
    let ioAToB = IO.async(onDone => onDone(Result.ok(a => a ++ "1")))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO (error)",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(_b => Result.error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual(Result.error("error"))->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("async (error)", () => {
    let ioAToB = IO.async(onDone => onDone(Result.error("error")))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )
  })

  describe("map", () => {
    let ioAToB = IO.map(one => a => a ++ one, IO.pure("1"))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("apply", () => {
    let ioAToB = IO.apply(IO.pure(one => a => a ++ one), IO.pure("1"))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })

  describe("flatMap", () => {
    let ioAToB = IO.flatMap(one => IO.pure(a => a ++ one), IO.pure("1"))

    testAsync(
      "pure",
      onDone => {
        let ioBToC = IO.pure(b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "throw",
      onDone => {
        let ioBToC = IO.throw("error")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone => {
        let ioBToC = IO.suspend(() => b => b ++ "2")

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspendIO",
      onDone => {
        let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (ok)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "async (error)",
      onDone => {
        let ioBToC = IO.async(onDone => onDone(Error("error")))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("fail"))
              | Error(e) => expect(e)->toEqual("error")->onDone
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone => {
        let ioBToC = IO.map(two => b => b ++ two, IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "apply",
      onDone => {
        let ioBToC = IO.apply(IO.pure(two => b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "flatMap",
      onDone => {
        let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"))

        IO.compose(ioBToC, ioAToB)->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a("0"))->toEqual("012")->onDone
              | Error(_) => onDone(fail("fail"))
              },
            _,
          )
        )
      },
    )
  })
})

describe("IO mapError", () => {
  testAsync("pure mapError unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.mapError(_ => "error", _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("throw mapError unsafeRunAsync", onDone =>
    IO.throw("this is a test")
    ->IO.mapError(msg => Relude_Js_Exn.make(msg), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_a) => onDone(fail("Failed"))
          | Error(_err) => onDone(pass)
          },
        _,
      )
    )
  )
})

describe("IO catchError", () => {
  testAsync("pure catchError unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.catchError((e: string) => IO.throw(e ++ e), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(42))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("throw catchError unsafeRunAsync", onDone =>
    IO.throw("42")
    ->IO.catchError(
      (e: string) => {
        let intValue = Relude.Int.fromString(e)->(Relude.Option.getOrElse(0, _))
        IO.throw(intValue * 2)
      },
      _,
    )
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(v) => onDone(expect(v)->toEqual(84))
          },
        _,
      )
    )
  )

  testAsync("pure flatMap catchError unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flatMap(a => IO.throw(string_of_int(a)), _)
    ->IO.catchError(_ => IO.pure(55), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(55))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )

  testAsync("async (ok) catchError unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok("0")))
    ->IO.catchError((e: string) => IO.throw(e ++ "1"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual("0"))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("async (error) catchError unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.error("0")))
    ->IO.catchError((e: string) => IO.throw(e ++ "1"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual("01"))
          },
        _,
      )
    )
  )

  describe("map catchError unsafeRunAsync", () => {
    let r0ToA = a => a ++ "1"

    testAsync(
      "pure",
      onDone =>
        IO.map(r0ToA, IO.pure("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.map(r0ToA, IO.throw("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.map(r0ToA, IO.suspend(() => "0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.map(r0ToA, IO.suspendIO(() => IO.pure("0")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.map(r0ToA, IO.async(onDone => onDone(Result.ok("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone =>
        IO.map(r0ToA, IO.async(onDone => onDone(Result.error("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(r0ToA, IO.map(a => a ++ "0", IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.map(r0ToA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.map(r0ToA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )
  })

  describe("apply catchError unsafeRunAsync", () => {
    let ioR0ToA = IO.pure(a => a ++ "1")

    testAsync(
      "pure",
      onDone =>
        IO.apply(ioR0ToA, IO.pure("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.apply(ioR0ToA, IO.throw("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.apply(ioR0ToA, IO.suspend(() => "0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.apply(ioR0ToA, IO.suspendIO(() => IO.pure("0")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.apply(ioR0ToA, IO.async(onDone => onDone(Result.ok("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone =>
        IO.apply(ioR0ToA, IO.async(onDone => onDone(Result.error("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.apply(ioR0ToA, IO.map(a => a ++ "0", IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(ioR0ToA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.apply(ioR0ToA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )
  })

  describe("flatMap catchError unsafeRunAsync", () => {
    let r0ToIOA = a => IO.pure(a ++ "1")

    testAsync(
      "pure",
      onDone =>
        IO.flatMap(r0ToIOA, IO.pure("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.flatMap(r0ToIOA, IO.throw("0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.flatMap(r0ToIOA, IO.suspend(() => "0"))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.flatMap(r0ToIOA, IO.suspendIO(() => IO.pure("0")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.flatMap(r0ToIOA, IO.async(onDone => onDone(Result.ok("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone =>
        IO.flatMap(r0ToIOA, IO.async(onDone => onDone(Result.error("0"))))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual("02"))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.flatMap(r0ToIOA, IO.map(a => a ++ "0", IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.flatMap(r0ToIOA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(r0ToIOA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
        ->IO.catchError((e: string) => IO.throw(e ++ "2"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual("+01"))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )
  })
})

describe("IO handleError", () =>
  testAsync("throw handleError unsafeRunAsync", onDone =>
    IO.throw("42")
    ->IO.handleError(e => int_of_string(e), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(42))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )
)

describe("IO mapHandleError", () => {
  testAsync("pure mapHandleError unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.mapHandleError(a => a * 2, int_of_string, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(b) => onDone(expect(b)->toEqual(84))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )

  testAsync("pure mapHandleError unsafeRunAsync", onDone =>
    IO.throw("42")
    ->IO.mapHandleError(a => a * 2, int_of_string, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(b) => onDone(expect(b)->toEqual(42))
          | Error(_) => onDone(fail("Fail"))
          },
        _,
      )
    )
  )
})

describe("IO bimap/bitap", () => {
  testAsync("suspend bimap bimap unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    ->IO.bimap(a => a * 2, e => e ++ e, _)
    ->IO.bimap(a => expect(a)->toEqual(84), _ => fail("fail"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspend bitap (ok) unsafeRunAsync", onDone => {
    let a = ref(0)

    IO.pure(42)
    ->IO.bitap(b => a := b + 1, e => a := e - 1, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(expect(a.contents)->toEqual(43))
          | Error(_) => onDone(fail("fail"))
          },
        _,
      )
    )
  })

  testAsync("suspend bitap (error) unsafeRunAsync", onDone => {
    let a = ref(0)

    IO.throw(42)
    ->IO.bitap(b => a := b + 1, e => a := e - 1, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("fail"))
          | Error(_) => onDone(expect(a.contents)->toEqual(41))
          },
        _,
      )
    )
  })
})

describe("IO alt", () => {
  testAsync("alt success success", onDone => {
    let a = ref(false)
    let b = ref(false)

    IO.alt(IO.suspend(() => a := true), IO.suspend(() => b := true))
    ->IO.bimap(
      _ => expect((a.contents, b.contents))->toEqual((true, false)),
      _ => fail("Failed"),
      _,
    )
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })

  testAsync("alt fail success", onDone => {
    let a = ref(false)
    let b = ref(false)

    IO.alt(
      IO.suspendIO(
        () => {
          a := true
          IO.throw("Failed!")
        },
      ),
      IO.suspend(() => b := true),
    )
    ->IO.bimap(_ => expect((a.contents, b.contents))->toEqual((true, true)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })

  testAsync("orElse success success", onDone => {
    let a = ref(false)
    let b = ref(false)
    IO.suspend(() => a := true)
    ->IO.orElse(~fallback=IO.suspend(() => b := true), _)
    ->IO.bimap(
      _ => expect((a.contents, b.contents))->toEqual((true, false)),
      _ => fail("Failed"),
      _,
    )
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })

  testAsync("orElse fail success", onDone => {
    let a = ref(false)
    let b = ref(false)
    IO.suspendIO(
      () => {
        a := true
        IO.throw("Failed!")
      },
    )
    ->IO.orElse(~fallback=IO.suspend(() => b := true), _)
    ->IO.bimap(_ => expect((a.contents, b.contents))->toEqual((true, true)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })

  testAsync("<|> alt operator success success", onDone => {
    let a = ref(false)
    let b = ref(false)
    module IOE = IO.WithError({
      type t = string
    })
    open IOE.Infix
    \"<|>"(IO.suspend(() => a := true), IO.suspend(() => b := true))
    ->IO.bimap(
      _ => expect((a.contents, b.contents))->toEqual((true, false)),
      _ => fail("Failed"),
      _,
    )
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })

  testAsync("<|> alt operator fail success", onDone => {
    let a = ref(false)
    let b = ref(false)
    module IOE = IO.WithError({
      type t = string
    })
    open IOE.Infix
    \"<|>"(
      IO.suspendIO(
        () => {
          a := true
          IO.throw("Darn")
        },
      ),
      IO.suspend(() => b := true),
    )
    ->IO.bimap(_ => expect((a.contents, b.contents))->toEqual((true, true)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })
})

describe("IO tries/exceptions", () => {
  testAsync("tries unsafeRunAsync", onDone =>
    IO.tries(throwJSError)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Should not be Ok"))
          | Error(Js.Exn.Error(jsExn)) =>
            let msg = Js.Exn.message(jsExn)
            onDone(expect(msg)->toEqual(Some("Error from JS")))
          | Error(_) => onDone(fail("Should have been an Js.Exn"))
          },
        _,
      )
    )
  )

  testAsync("triesJS unsafeRunAsync", onDone =>
    IO.triesJS(throwJSError)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Should not be Ok"))
          | Error(jsExn) =>
            let msg = Js.Exn.message(jsExn)
            onDone(expect(msg)->toEqual(Some("Error from JS")))
          },
        _,
      )
    )
  )

  testAsync("triesJS with Reason Js.Exn.raiseError", onDone =>
    IO.triesJS(() => Js.Exn.raiseError("Fail"))->(
      IO.unsafeRunAsync(
        result =>
          switch result {
          | Ok(_) => onDone(fail("Should not be Ok"))
          | Error(e) => onDone(expect(Js.Exn.message(e))->toEqual(Some("Fail")))
          },
        _,
      )
    )
  )

  testAsync("triesJS with raw JS function that throws", onDone => {
    let jsThrow: unit => unit = %raw(`
      function() {
        throw new Error("This sucks");
      }
    `)

    IO.triesJS(() => jsThrow())->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Should not be Ok"))
          | Error(e) => onDone(expect(Js.Exn.message(e))->toEqual(Some("This sucks")))
          },
        _,
      )
    )
  })

  // Something must have changed in how custom exceptions are raised from OCaml,
  // because the message inside the error appears to be `[Object object]`. For
  // now, we can just skip this test.
  Skip.testAsync("triesJS with exn", onDone => {
    exception MyExn(string)

    IO.triesJS(() => raise(MyExn("Custom error")))->(
      IO.unsafeRunAsync(
        result =>
          switch result {
          | Ok(_) => onDone(fail("Should not be Ok"))
          | Error(e) =>
            Js.log(e)
            onDone(
              expect(Js.Exn.message(e))->toEqual(Some("Unexpected error: MyExn,8,Custom error")),
            )
          },
        _,
      )
    )
  })
})

describe("IO flip", () => {
  testAsync("pure flip unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual(42))
          },
        _,
      )
    )
  )

  testAsync("throw flip unsafeRunAsync", onDone =>
    IO.throw("my error")
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual("my error"))
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("suspend flip unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual(42))
          },
        _,
      )
    )
  )

  testAsync("suspendIO flip unsafeRunAsync", onDone =>
    IO.suspendIO(() => Pure(42))
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual(42))
          },
        _,
      )
    )
  )

  testAsync("async flip unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Ok(42)))
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual(42))
          },
        _,
      )
    )
  )

  describe("map flip unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.pure(42)
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.throw(42)
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual(42))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 42)
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(42))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.async(onDone => onDone(Result.ok(42)))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone =>
        IO.async(onDone => onDone(Result.error(42)))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual(42))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(b => b + 42, IO.pure(0))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(b => b + 42), IO.pure(0))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(b => IO.pure(b + 42), IO.pure(0))
        ->IO.map(a => a + 10, _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )
  })

  describe("apply flip unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.pure(42)
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.throw(42)
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual(42))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 42)
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(42))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.async(onDone => onDone(Result.ok(42)))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone =>
        IO.async(onDone => onDone(Result.error(42)))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual(42))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(b => b + 42, IO.pure(0))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(b => b + 42), IO.pure(0))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(b => b + 42, IO.pure(0))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(b => IO.pure(b + 42), IO.pure(0))
        ->IO.apply(IO.pure(a => a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )
  })

  testAsync("pure flatMap flip unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.flatMap(a => Pure(a + 10), _)
    ->IO.flip
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(fail("Failed"))
          | Error(e) => onDone(expect(e)->toEqual(52))
          },
        _,
      )
    )
  )

  describe("flatMap flip unsafeRunAsync", () => {
    testAsync(
      "pure flatMap map",
      onDone =>
        IO.pure(42)
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flatMap(a => Pure(a + 100), _)
        ->IO.map(a => a + 1000, _)
        ->IO.flatMap(a => IO.async(onDone => onDone(Result.ok(a + 10000))), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(11152))
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone =>
        IO.throw(42)
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => onDone(expect(a)->toEqual(42))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 42)
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(42))
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "async flatMap suspend flip bimap unsafeRunAsync",
      onDone =>
        IO.async(onDone => onDone(Result.ok(42)))
        ->IO.flatMap(a => IO.suspend(() => a), _)
        ->IO.flip
        ->IO.bimap(_ => fail("fail"), e => expect(e)->toEqual(42), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => a + 42, IO.pure(0))
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(a => a + 42), IO.pure(0))
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(a => IO.pure(a + 42), IO.pure(0))
        ->IO.flatMap(a => Pure(a + 10), _)
        ->IO.flip
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(fail("Failed"))
              | Error(e) => onDone(expect(e)->toEqual(52))
              },
            _,
          )
        ),
    )
  })
})

describe("IO summonError", () => {
  testAsync("pure summonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Ok(42)), _e => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspend summonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Ok(42)), _e => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspendIO pure summonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(42))
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Ok(42)), _e => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspendIO throw summonError unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.throw("error!"))
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Error("error!")), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspendIO pure map flatMap pure summonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(42)->IO.map(a => a + 10, _)->(IO.flatMap(a => IO.pure(a + 11), _)))
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Ok(63)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("async flatMap summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => 42->Result.ok->onDone)
    ->IO.flatMap(IO.pure, _)
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Ok(42)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("async flatMap throw summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => 42->Result.ok->onDone)
    ->IO.flatMap(IO.throw, _)
    ->IO.summonError
    ->IO.bimap(resA => expect(resA)->toEqual(Error(42)), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("async flatMap suspend summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok(42)))
    ->IO.flatMap(a => IO.suspend(() => a), _)
    ->IO.summonError
    ->IO.bimap(res => expect(res)->toEqual(Ok(42)), Relude.Void.absurd, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  describe("map unsafeRunAsync", () => {
    testAsync(
      "throw",
      onDone =>
        IO.map(a => a + 42, IO.throw(1))
        ->IO.summonError
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(a) => expect(a->Result.getError)->toEqual(Some(1))->onDone
              | Error(_) => fail("Failed")->onDone
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.map(a => a + 42, IO.suspend(() => 1))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(43)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.map(a => a + 42, IO.suspendIO(() => IO.pure(1)))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(43)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async",
      onDone =>
        IO.map(a => a + 42, IO.async(onDone => onDone(Result.ok(1))))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(43)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => a + 42, IO.map(b => b + 2, IO.pure(1)))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(45)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.map(a => a + 42, IO.apply(IO.pure(b => b + 2), IO.pure(1)))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(45)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.map(a => a + 42, IO.flatMap(b => IO.pure(b + 2), IO.pure(1)))
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(45)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )
  })

  describe("apply unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.pure(42)
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone => {
        let a = ref(None)

        IO.throw(2)
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.tap(b => a := b->Result.getError, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(expect(a.contents)->toEqual(Some(2)))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 42)
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(42))
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.async(onDone => Ok(42)->onDone)
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone => {
        let a = ref(None)

        IO.async(onDone => Error(42)->onDone)
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.tap(b => a := b->Result.getError, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(expect(a.contents)->toEqual(Some(42)))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => a + 1, IO.pure(41))
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(a => a + 1), IO.pure(41))
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(a => IO.pure(a + 1), IO.pure(41))
        ->IO.apply(IO.pure(a => a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )
  })

  describe("flatMap unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.pure(42)
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "throw",
      onDone => {
        let a = ref(None)

        IO.throw(2)
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.tap(b => a := b->Result.getError, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(expect(a.contents)->toEqual(Some(2)))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 42)
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(42))
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.async(onDone => Ok(42)->onDone)
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async (error)",
      onDone => {
        let a = ref(None)

        IO.async(onDone => Error(42)->onDone)
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.tap(b => a := b->Result.getError, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(_) => onDone(expect(a.contents)->toEqual(Some(42)))
              | Error(_) => onDone(fail("Failed"))
              },
            _,
          )
        )
      },
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => a + 1, IO.pure(41))
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(a => a + 1), IO.pure(41))
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(a => IO.pure(a + 1), IO.pure(41))
        ->IO.flatMap(a => IO.pure(a * 2), _)
        ->IO.summonError
        ->IO.bimap(res => expect(res)->toEqual(Ok(84)), Relude.Void.absurd, _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )
  })
})

describe("IO unsummonError", () => {
  testAsync("pure Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Ok(42))
    ->IO.unsummonError
    ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Error("e!"))
    ->IO.unsummonError
    ->IO.bimap(_ => fail("Failed"), error => expect(error)->toEqual("e!"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspend Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => Ok(42))
    ->IO.unsummonError
    ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspend Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => Error("e!"))
    ->IO.unsummonError
    ->IO.bimap(_ => fail("Failed"), error => expect(error)->toEqual("e!"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspendIO pure Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(Ok(42)))
    ->IO.unsummonError
    ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("suspendIO pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(Error("e!")))
    ->IO.unsummonError
    ->IO.bimap(_ => fail("Failed"), error => expect(error)->toEqual("e!"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  describe("flatMap unsummonError unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.pure(0)
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.suspend(() => 0)
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.suspendIO(() => IO.pure(0))
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async flatMap suspend unsummonError bimap unsafeRunAsync",
      onDone =>
        IO.async(onDone => onDone(Result.ok(Result.ok(42))))
        ->IO.flatMap(a => IO.suspend(() => a), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(42), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => a + 1, IO.pure(0))
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(43), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.apply(IO.pure(a => a + 1), IO.pure(0))
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(43), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.flatMap(a => IO.pure(a + 1), IO.pure(0))
        ->IO.flatMap(a => IO.pure(Ok(a + 42)), _)
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(43), _ => fail("fail"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )
  })

  describe("map unsummonError bimap unsafeRunAsync", () => {
    testAsync(
      "pure",
      onDone =>
        IO.map(a => Ok(a + 1), IO.pure(0))
        ->IO.unsummonError
        ->IO.bimap(_ => pass, _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspend",
      onDone =>
        IO.map(a => Ok(a + 1), IO.suspend(() => 0))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(1), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "suspendIO",
      onDone =>
        IO.map(a => Ok(a + 1), IO.suspendIO(() => IO.pure(0)))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(1), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "async (ok)",
      onDone =>
        IO.map(a => Ok(a + 1), IO.async(onDone => onDone(Ok(0))))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(1), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "map",
      onDone =>
        IO.map(a => Ok(a + 1), IO.map(a => a + 1, IO.pure(0)))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(2), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "apply",
      onDone =>
        IO.map(a => Ok(a + 1), IO.apply(IO.pure(a => a + 1), IO.pure(0)))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(2), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )

    testAsync(
      "flatMap",
      onDone =>
        IO.map(a => Ok(a + 1), IO.flatMap(a => IO.pure(a + 1), IO.pure(0)))
        ->IO.unsummonError
        ->IO.bimap(a => expect(a)->toEqual(2), _ => fail("Failed"), _)
        ->(
          IO.unsafeRunAsync(
            x =>
              switch x {
              | Ok(assertion) => onDone(assertion)
              | Error(assertion) => onDone(assertion)
              },
            _,
          )
        ),
    )
  })

  testAsync("pure map flatMap pure summonError unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.map(a => a + 10, _)
    ->IO.flatMap(a => IO.pure(a + 11), _)
    ->IO.summonError
    ->IO.unsummonError
    ->IO.bimap(a => expect(a)->toEqual(63), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  )

  testAsync("pure apply summonError apply unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    ->IO.apply(IO.pure(a => a * 2), _)
    ->IO.summonError
    ->IO.apply(IO.pure(res => res->(Result.map(a => a * 3, _))), _)
    ->IO.unsummonError
    ->IO.bimap(a => expect(a)->toEqual(252), _ => fail("Failed"), _)
    ->(IO.unsafeRunAsync(res => res->Result.merge->onDone, _))
  )
})

describe("IO delay", () => {
  testAsync("delay unsafeRunAsync", onDone => {
    IO.delay(10)->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(pass)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )

    Jest.advanceTimersByTime(10)
  })

  testAsync("pure withDelay unsafeRunAsync", onDone => {
    IO.pure(42)
    ->IO.withDelay(10, _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(a) => onDone(expect(a)->toEqual(42))
          | Error(_) => onDone(fail("fail"))
          },
        _,
      )
    )

    Jest.advanceTimersByTime(10)
  })
})

testAsync("IO delayWithVoid unsafeRunAsync", onDone => {
  IO.delayWithVoid(10)->(IO.unsafeRunAsync(x =>
      switch x {
      | Ok(_) => onDone(pass)
      | Error(_) => onDone(fail("Failed"))
      }
    , _))
  Jest.advanceTimersByTime(10)
})

testAsync("IO withDelayBefore unsafeRunAsync", onDone => {
  IO.pure(0)
  ->IO.withDelayBefore(10, _)
  ->(IO.unsafeRunAsync(x =>
      switch x {
      | Ok(_) => onDone(pass)
      | Error(_) => onDone(fail("Failed"))
      }
    , _))
  Jest.advanceTimersByTime(10)
})

describe("IO debounce", () => {
  // TODO: need to use fake timers
  Skip.testAsync("debounce", onDone => {
    // This will test that when a debounced IO is called, it will only let the most recent one go through
    // after some predetermined amount of time. After that call has gone through the time should reset and
    // the next time the function is called it will have to wait that amount of time all over again.
    let getTimestamp = () => Js.Date.make()->Js.Date.getTime
    let timeIntervals = ref(list{getTimestamp()})
    let intervalMs = 100
    let areTimestampsSpacedCorrectly = (x1, x2) => x2 -. x1 >= intervalMs->float_of_int
    let debouncedIO =
      IO.debounce(
        ~intervalMs,
        () => {
          timeIntervals := list{getTimestamp(), ...timeIntervals.contents}
          IO.pure()
        },
        ...
      )

    let checkNonRunIO = IO.unsafeRunAsync(
      Result.fold(
        _ => "IO should not have failed"->fail->onDone,
        Relude_Option.foldLazy(ignore, () => "IO should not have been run"->fail->onDone, _),
        _,
      ),
      _,
    )

    debouncedIO()->checkNonRunIO

    debouncedIO()->checkNonRunIO

    debouncedIO()->IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO, _), _)->checkNonRunIO

    debouncedIO()
    ->IO.flatMap(\">>"(ignore, debouncedIO, _), _)
    ->IO.flatMap(_ => IO.delay(300), _)
    ->(
      IO.unsafeRunAsync(
        _ =>
          switch timeIntervals.contents {
          | list{x2, x1, x0}
            if areTimestampsSpacedCorrectly(x0, x1) && areTimestampsSpacedCorrectly(x1, x2) => pass
          | list{_, _, _} => fail("debounced IO did not time executions correctly")
          | list{_}
          | list{_, _} =>
            fail("debounced IO was executed too few times")
          | xs =>
            fail(
              "debounced IO was executed too many times. Was executed " ++
              (xs->List.length - 1)->string_of_int,
            )
          }->onDone,
        _,
      )
    )
  })

  // TODO: need to use fake timers
  Skip.testAsync("debounce immediate", onDone => {
    // This tests a debounced IO that has an execution on the leading edge of the timing interval. It will
    // test that the first execution immediately goes out and that only the latest execution that happens
    // within the time interval goes out. After that time interval is up, the next time the IO is executed it
    // should go out immediately
    let getTimestamp = () => Js.Date.make()->Js.Date.getTime
    let timeIntervals = ref(list{getTimestamp()})
    let intervalMs = 100
    let areTimestampsSpacedCorrectly = (x1, x2) => x2 -. x1 >= intervalMs->float_of_int

    let debouncedIO =
      IO.debounce(
        ~immediate=true,
        ~intervalMs,
        () => {
          timeIntervals := list{getTimestamp(), ...timeIntervals.contents}
          IO.pure()
        },
        ...
      )

    debouncedIO()->(IO.unsafeRunAsync(ignore, _))

    debouncedIO()->(IO.unsafeRunAsync(ignore, _))

    debouncedIO()
    ->IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO, _), _)
    ->(IO.unsafeRunAsync(ignore, _))

    debouncedIO()
    ->IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO, _), _)
    ->IO.flatMap(_ => IO.delay(300), _)
    ->(
      IO.unsafeRunAsync(
        _ =>
          switch timeIntervals.contents {
          | list{x3, x2, x1, x0}
            if Float.approximatelyEqual(~tolerance=2.0, x1, x0) &&
            (areTimestampsSpacedCorrectly(x1, x2) &&
            Float.approximatelyEqual(~tolerance=2.0, x2, x3)) => pass
          | list{_, _, _, _} => fail("debounced IO did not time executions correctly")
          | list{_}
          | list{_, _} =>
            fail("debounced IO was executions too few times")
          | xs =>
            fail(
              "debounced IO was executions too many times. Was executed " ++
              (xs->List.length - 1)->string_of_int,
            )
          }->onDone,
        _,
      )
    )
  })
})

describe("IO throttle", () => {
  // TODO: need to use fake timers
  Skip.testAsync("throttle", onDone => {
    let getTimestamp = () => Js.Date.make()->Js.Date.getTime
    let timeIntervals = ref(list{getTimestamp()})
    let intervalMs = 100
    let throttledIO =
      IO.throttle(
        ~intervalMs,
        () => {
          timeIntervals := list{getTimestamp(), ...timeIntervals.contents}
          IO.pure()
        },
        ...
      )

    throttledIO()->(IO.unsafeRunAsync(ignore, _))

    throttledIO()->(IO.unsafeRunAsync(ignore, _))

    throttledIO()->IO.flatMap(\">>"(ignore, throttledIO, _), _)->(IO.unsafeRunAsync(ignore, _))

    IO.delay(300)
    ->IO.flatMap(throttledIO, _)
    ->(
      IO.unsafeRunAsync(
        _ =>
          switch timeIntervals.contents {
          | list{x2, x1, x0}
            if x2 -. x1 >= intervalMs->float_of_int &&
              Float.approximatelyEqual(~tolerance=2.0, x1, x0) => pass
          | list{_, _, _} => fail("throttled IO did not time executions correctly")
          | list{_}
          | list{_, _} =>
            fail("throttled IO was executed too few times")
          | xs =>
            fail(
              "throttled IO was executed too many times. Was executed " ++
              (xs->List.length - 1)->string_of_int,
            )
          }->onDone,
        _,
      )
    )
  })

  testAsync("all", onDone => {
    module IOE = IO.WithError({
      type t = string
    })
    list{IO.pure(1), IO.pure(2), IO.pure(3)}
    ->IOE.all
    ->IO.bimap(a => expect(a)->toEqual(list{1, 2, 3}), _ => fail("Failed"), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(assertion) => onDone(assertion)
          },
        _,
      )
    )
  })
})

describe("IO parallel", () =>
  testAsync("parallel", onDone => {
    module IOE = IO.WithError({
      type t = string
    })

    let a = ref(false)
    let b = ref(false)
    let c = ref(false)

    let ioA =
      IO.suspend(
        () => {
          a := true
          (a.contents, b.contents, c.contents)
        },
      )
      ->IO.withDelay(100, _)
      ->(
        IO.map(
          t => {
            a := false
            (t, (a.contents, b.contents, c.contents))
          },
          _,
        )
      )

    let ioB =
      IO.suspend(
        () => {
          b := true
          (a.contents, b.contents, c.contents)
        },
      )
      ->IO.withDelay(100, _)
      ->(
        IO.map(
          t => {
            b := false
            (t, (a.contents, b.contents, c.contents))
          },
          _,
        )
      )

    let ioC =
      IO.suspend(
        () => {
          c := true
          (a.contents, b.contents, c.contents)
        },
      )
      ->IO.withDelay(100, _)
      ->(
        IO.map(
          t => {
            c := false
            (t, (a.contents, b.contents, c.contents))
          },
          _,
        )
      )

    let a0 = (a.contents, b.contents, c.contents)

    // before starting, none are running
    // when a is run, a sees itself running, but not yet b and c
    // after delay, a completes, but sees b and c still running (they have not completed yet)
    // when b is run, b sees both a and b running, but not yet c
    // after delay, b completes, but sees c still running (it has not completed yet)
    // when c is run c sees all a, b, and c running
    // after delay c completes and sees a, b, c not running

    // before starting, none are running
    // when a is run, a sees itself running, but not yet b and c
    // after delay, a completes, but sees b and c still running (they have not completed yet)
    // when b is run, b sees both a and b running, but not yet c
    // after delay, b completes, but sees c still running (it has not completed yet)
    // when c is run c sees all a, b, and c running
    // after delay c completes and sees a, b, c not running
    let ioAll =
      (ioA, ioB, ioC)->(
        IOE.mapTuple3(
          ((a1, a2), (b1, b2), (c1, c2)) =>
            expect((a0, a1, a2, b1, b2, c1, c2))->toEqual((
              (false, false, false),
              (true, false, false),
              (false, true, true),
              (true, true, false),
              (false, false, true),
              (true, true, true),
              (false, false, false),
            )),
          _,
        )
      )

    ioAll->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )

    // We should only need to advance by a total of 100ms to get all the IOs to
    // start and complete (if running in parallel)
    Jest.advanceTimersByTime(50)
    Jest.advanceTimersByTime(100)
  })
)

type getError = GetError(string)

type parseError = ParseError(string)

type printError = PrintError(string)

type appError =
  | EGet(getError)
  | EParse(parseError)
  | EPrint(printError)

let eGet = e => EGet(e)
let eParse = e => EParse(e)
let ePrint = e => EPrint(e)

module AppErrorType: BsBastet.Interface.TYPE with type t = appError = {
  type t = appError
}
module IOAppError = IO.WithError(AppErrorType)

let \">>=" = IOAppError.Infix.\">>="
let \">=>" = IOAppError.Infix.\">=>"

let getData: IO.t<string, getError> = IO.suspendIO(() => IO.pure("data"))

let parseData: string => IO.t<int, parseError> = data => {
  let l = Relude.String.length(data)
  if l > 0 {
    IO.pure(l)
  } else {
    IO.throw(ParseError("Bad data: " ++ data))
  }
}

let printNumber: int => IO.t<unit, printError> = _num => IO.unit

describe("IO realish examples", () => {
  testAsync("example >>=", onDone =>
    \">>="(
      \">>="(
        \">>="(getData->(IO.mapError(eGet, _)), \">>"(parseData, IO.mapError(eParse, _), _)),
        \">>"(printNumber, IO.mapError(ePrint, _), _),
      ),
      _ => IO.pure(pass),
    )->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("example >=>", onDone => {
    let getIO = \">=>"(
      \">=>"(
        \">=>"(
          _ => getData->(IO.mapError(eGet, _)),
          \">>"(parseData, IO.mapError(eParse, _), _),
          _,
        ),
        \">>"(printNumber, IO.mapError(ePrint, _), _),
        _,
      ),
      _ => IO.pure(pass),
      _,
    )

    getIO()->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  })

  testAsync("example flatMap", onDone =>
    getData
    ->IO.mapError(e => EGet(e), _)
    ->IO.flatMap(str => parseData(str)->(IO.mapError(e => EParse(e), _)), _)
    ->IO.flatMap(num => printNumber(num)->(IO.mapError(e => EPrint(e), _)), _)
    ->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(_) => onDone(pass)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )
})

let testFilePath = FS.testFilePath("Eff_test.txt")

module JsExnType: BsBastet.Interface.TYPE with type t = Js.Exn.t = {
  type t = Js.Exn.t
}
module IOJsExn = IO.WithError(JsExnType)

let \">>=" = IOJsExn.Infix.\">>="
let \">=>" = IOJsExn.Infix.\">=>"

describe("IO FS examples", () => {
  beforeAll(() => FS.IO.writeFileSync(testFilePath, "")->(IO.unsafeRunAsync(ignore, _)))

  testAsync("read and writeFileSync", onDone =>
    \">>="(
      \">>="(
        FS.IO.writeFileSync(testFilePath, "IO Eff test"),
        _ => FS.IO.readFileSync(testFilePath),
      ),
      content => IO.pure(expect(content)->toEqual("IO Eff test")),
    )->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(_jsExn) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )

  testAsync("readFile", onDone =>
    \">>="(
      \">>="(
        \">>="(FS.IO.writeFile(testFilePath, "IO Aff test"), _ => FS.IO.readFile(testFilePath)),
        content =>
          Relude_String.toNonWhitespace(content)->(
            IO.fromOption(_ => Relude_Js_Exn.make("Failed to get non-empty file content"), _)
          ),
      ),
      content => IO.pure(expect(content)->toEqual("IO Aff test")),
    )->(
      IO.unsafeRunAsync(
        x =>
          switch x {
          | Ok(assertion) => onDone(assertion)
          | Error(_) => onDone(fail("Failed"))
          },
        _,
      )
    )
  )
})
