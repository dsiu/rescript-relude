open Jest
open Expect

module StateT = Relude.StateT

module State = StateT.State.WithState({
  type t = list<int>
})

let (\"<$>", \"$>", \"<$$>", \"*>", \">>=") = {
  open State.Infix
  (\"<$>", \"$>", \"<$$>", \"*>", \">>=")
}

module Stack = {
  let push: int => StateT.State.t<int, list<int>> = x =>
    \"$>"(StateT.State.modify(xs => list{x, ...xs}), x)

  let pop: StateT.State.t<option<int>, list<int>> = \">>="(State.get, values =>
    switch values {
    | list{} => \"$>"(State.put(list{}), None)
    | list{x, ...xs} => \"$>"(State.put(xs), Some(x))
    }
  )
}

describe("StateT", () => {
  test("pure", () => {
    let result = State.pure(2) |> State.runStateT(list{})
    expect(result) -> toEqual((2, list{}))
  })

  test("put", () => {
    let result = \">>="(State.pure(2), a => \"$>"(State.put(list{a}), a)) |> State.runStateT(list{})
    expect(result) -> toEqual((2, list{2}))
  })

  test("stack example 1 (push)", () => {
    let result = \">>="(Stack.push(1), _ => Stack.push(2)) |> State.runStateT(list{})
    expect(result) -> toEqual((2, list{2, 1}))
  })

  test("stack example 2 (push, pop)", () => {
    let result =
      \">>="(Stack.push(1), _ =>
        \">>="(Stack.push(2), _ => \">>="(Stack.push(3), _ => Stack.pop))
      ) |> State.runStateT(list{})
    expect(result) -> toEqual((Some(3), list{2, 1}))
  })

  test("stack example 3", () => {
    // do notation :(
    let result =
      \">>="(Stack.push(1), _ =>
        \">>="(Stack.push(2), _ =>
          \">>="(Stack.push(3), _ =>
            \">>="(Stack.pop, _ =>
              \">>="(Stack.pop, _ => \">>="(Stack.push(4), _ => Stack.push(5)))
            )
          )
        )
      ) |> State.runStateT(list{})
    expect(result) -> toEqual((5, list{5, 4, 1}))
  })

  test("stack example 4", () => {
    // do notation :(
    let result =
      \">>="(
        \">>="(
          \">>="(
            \">>="(\">>="(\">>="(Stack.push(1), _ => Stack.push(2)), _ => Stack.push(3)), _ =>
              Stack.pop
            ),
            _ => Stack.pop,
          ),
          _ => Stack.push(4),
        ),
        _ => \"<$$>"(Stack.push(5), a => a * 100),
      ) |> State.runStateT(list{})
    expect(result) -> toEqual((500, list{5, 4, 1}))
  })

  test("*> loses state", () => {
    let result =
      \"*>"(\"*>"(Stack.push(1), Stack.push(2)), Stack.push(3)) |> State.runStateT(list{})
    // Not 100% sure if this is expected behavior, but the applicative behavior throws away the
    // state on the left side here.  It makes sense, because there is no attempt to merge the states
    // in apply.  I'll have to compare this with purescript/haskell to be sure.
    expect(result) -> toEqual((3, list{3}))
  })
})
