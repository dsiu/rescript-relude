open Jest
open Expect

module RWST = Relude.RWST
module Identity = Relude.Identity

// Define our Reader environment type (configuration)
module Env = {
  type t = {multiplier: int}
}

// Define our State type (counter)
module State = {
  type t = int
}

// Define our Writer log type (list of strings)
module Log = Relude.WriterT.WriterLog.List.WithEntry({
  type t = string
})

// Create RWST with Identity monad
module RWS = RWST.WithMonad(Identity.Monad)

// Create the concrete RWST instance
module RWST_Instance = RWS.WithEnvAndStateAndLog(Env, State, Log)

// Helper functions for common RWST operations
// Get the reader environment
let ask: RWS.t<Env.t, Env.t, State.t, Log.t> = RWS.RWST((r, s) =>
  Identity.pure(RWST.RWSResult.RWSResult(r, s, list{}))
)

// Get the current state
let get: RWS.t<State.t, Env.t, State.t, Log.t> = RWS.RWST((_r, s) =>
  Identity.pure(RWST.RWSResult.RWSResult(s, s, list{}))
)

// Set the state
let put: State.t => RWS.t<unit, Env.t, State.t, Log.t> = newState =>
  RWS.RWST((_r, _s) => Identity.pure(RWST.RWSResult.RWSResult((), newState, list{})))

// Modify the state
let modify: (State.t => State.t) => RWS.t<unit, Env.t, State.t, Log.t> = f =>
  RWS.RWST((_r, s) => Identity.pure(RWST.RWSResult.RWSResult((), f(s), list{})))

// Write to the log
let tell: list<string> => RWS.t<unit, Env.t, State.t, Log.t> = log =>
  RWS.RWST((_r, s) => Identity.pure(RWST.RWSResult.RWSResult((), s, log)))


let (\"<$>", \"$>", \"<$$>", \"*>", \">>=") = {
  open RWST_Instance.Infix
  (\"<$>", \"$>", \"<$$>", \"*>", \">>=")
}

// Helper to run RWST and extract the result
let run = (env, state, rwst) => {
  let RWST.RWSResult.RWSResult(a, s, w) = RWST_Instance.runRWST(env, state, rwst)
  (a, s, w)
}

describe("RWST", () => {
  test("pure", () => {
    let rwst = RWST_Instance.pure(42)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((42, 0, list{}))
  })

  test("map", () => {
    let rwst = \"<$$>"(RWST_Instance.pure(10), x => x * 2)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((20, 0, list{}))
  })

  test("apply", () => {
    let f = RWST_Instance.pure(x => x + 10)
    let a = RWST_Instance.pure(5)
    let rwst = RWST_Instance.apply(f, a)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((15, 0, list{}))
  })

  test("bind (>>=)", () => {
    let rwst =
      RWST_Instance.pure(5)
      ->\">>="(a => RWST_Instance.pure(a * 2))
      ->\">>="(b => RWST_Instance.pure(b + 3))
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((13, 0, list{}))
  })

  test("ask - read environment", () => {
    let rwst = ask
    let result = run({Env.multiplier: 5}, 0, rwst)
    expect(result)->toEqual(({Env.multiplier: 5}, 0, list{}))
  })

  test("ask and use environment", () => {
    let rwst = ask->\">>="(env => \"<$$>"(RWST_Instance.pure(10), x => x * env.multiplier))
    let result = run({Env.multiplier: 3}, 0, rwst)
    expect(result)->toEqual((30, 0, list{}))
  })

  test("get - read state", () => {
    let rwst = get
    let result = run({Env.multiplier: 2}, 42, rwst)
    expect(result)->toEqual((42, 42, list{}))
  })

  test("put - write state", () => {
    let rwst = put(100)->\"$>"(42)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((42, 100, list{}))
  })

  test("modify - modify state", () => {
    let rwst = modify(s => s + 10)->\"$>"(42)
    let result = run({Env.multiplier: 2}, 5, rwst)
    expect(result)->toEqual((42, 15, list{}))
  })

  test("tell - write to log", () => {
    let rwst = tell(list{"hello"})->\"$>"(42)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((42, 0, list{"hello"}))
  })

  test("tell multiple times - log accumulates", () => {
    let rwst =
      tell(list{"first"})
      ->\">>="(_ => tell(list{"second"}))
      ->\">>="(_ => tell(list{"third"}))
      ->\"$>"(42)
    let result = run({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((42, 0, list{"first", "second", "third"}))
  })

  test("combined reader, writer, state", () => {
    let rwst =
      get
      ->\">>="(currentState => {
        let msg1 = "Initial state: " ++ Int.toString(currentState)
        tell(list{msg1})
        ->\">>="(_ =>
          ask->\">>="(env => {
            let msg2 = "Multiplier: " ++ Int.toString(env.multiplier)
            tell(list{msg2})
            ->\">>="(_ => {
              let newValue = currentState * env.multiplier
              let msg3 = "New state: " ++ Int.toString(newValue)
              put(newValue)->\">>="(_ => tell(list{msg3})->\"$>"(newValue))
            })
          })
        )
      })
    let result = run({Env.multiplier: 3}, 10, rwst)
    expect(result)->toEqual((
      30,
      30,
      list{"Initial state: 10", "Multiplier: 3", "New state: 30"},
    ))
  })

  test("evalRWST - return (result, log) only", () => {
    let rwst =
      get
      ->\">>="(s =>
        put(s + 10)->\">>="(_ => tell(list{"incremented"})->\"$>"(s + 10))
      )
    let result = RWST_Instance.evalRWST({Env.multiplier: 2}, 5, rwst)
    expect(result)->toEqual((15, list{"incremented"}))
  })

  test("execRWST - return (state, log) only", () => {
    let rwst =
      RWST_Instance.pure(42)
      ->\">>="(x => put(x)->\">>="(_ => tell(list{"set state"})->\"$>"(x * 2)))
    let result = RWST_Instance.execRWST({Env.multiplier: 2}, 0, rwst)
    expect(result)->toEqual((42, list{"set state"}))
  })

  test("mapRWST - transform the result", () => {
    let rwst = RWST_Instance.pure(10)
    let transformed = RWST_Instance.mapRWST(
      Identity.map(
        (RWST.RWSResult.RWSResult(a, s, w)) =>
          RWST.RWSResult.RWSResult(a * 2, s + 1, list{"transformed", ...w}),
        _,
      ),
      rwst,
    )
    let result = run({Env.multiplier: 2}, 5, transformed)
    expect(result)->toEqual((20, 6, list{"transformed"}))
  })

  test("withRWST - transform reader and state", () => {
    let rwst = ask->\">>="(env => \"<$$>"(get, s => s * env.multiplier))
    // Transform: double the multiplier, add 10 to state
    let transformed = RWST_Instance.withRWST(
      (env, s) => ({Env.multiplier: env.Env.multiplier * 2}, s + 10),
      rwst,
    )
    let result = run({Env.multiplier: 3}, 5, transformed)
    // Original would be: 5 * 3 = 15
    // Transformed: (5 + 10) * (3 * 2) = 15 * 6 = 90
    expect(result)->toEqual((90, 15, list{}))
  })

  test("complex stateful computation with logging", () => {
    // Simulate a counter that logs operations
    let increment = modify(x => x + 1)->\">>="(_ =>
      get->\">>="(newVal => {
        let msg = "Incremented to " ++ Int.toString(newVal)
        tell(list{msg})->\"$>"(newVal)
      })
    )

    let decrement = modify(x => x - 1)->\">>="(_ =>
      get->\">>="(newVal => {
        let msg = "Decremented to " ++ Int.toString(newVal)
        tell(list{msg})->\"$>"(newVal)
      })
    )

    let rwst =
      increment
      ->\">>="(_ => increment)
      ->\">>="(_ => decrement)
      ->\">>="(_ => get)

    let result = run({Env.multiplier: 1}, 0, rwst)
    expect(result)->toEqual((
      1,
      1,
      list{"Incremented to 1", "Incremented to 2", "Decremented to 1"},
    ))
  })

  test("using environment in stateful computation", () => {
    // Read config multiplier and multiply state by it
    let multiplyState =
      ask->\">>="(env =>
        get->\">>="(s => {
          let newVal = s * env.multiplier
          let msg =
            Int.toString(s) ++
            " * " ++
            Int.toString(env.multiplier) ++
            " = " ++
            Int.toString(newVal)
          put(newVal)->\">>="(_ => tell(list{msg})->\"$>"(newVal))
        })
      )

    let result = run({Env.multiplier: 7}, 6, multiplyState)
    expect(result)->toEqual((42, 42, list{"6 * 7 = 42"}))
  })
})