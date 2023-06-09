open Jest
open Expect

module Timer = Relude_Timer
module Debounce = Relude_Debounce

describe("Debounce", () => {
  test("debounce (leading=false)", () => {
    Jest.useFakeTimers()

    let runCount = ref(0)

    let f = () => runCount := runCount.contents + 1

    let debounced = Debounce.debounce(~delayMS=200, ~leading=false, f)

    let isScheduled1 = debounced.isScheduled()

    debounced.f()
    debounced.f()
    debounced.f()

    let isScheduled2 = debounced.isScheduled()

    Jest.advanceTimersByTime(100)
    let runCount1 = runCount.contents

    debounced.f()
    debounced.f()
    debounced.f()

    let isScheduled3 = debounced.isScheduled()

    Jest.advanceTimersByTime(199)
    let runCount2 = runCount.contents

    Jest.advanceTimersByTime(1)
    let runCount3 = runCount.contents

    let isScheduled4 = debounced.isScheduled()

    expect((
      runCount1,
      runCount2,
      runCount3,
      isScheduled1,
      isScheduled2,
      isScheduled3,
      isScheduled4,
    )) |> toEqual((0, 0, 1, false, true, true, false))
  })

  test("debounce (leading=true)", () => {
    Jest.useFakeTimers()

    let runCount = ref(0)

    let f = () => runCount := runCount.contents + 1

    let debounced = Debounce.debounce(~delayMS=200, ~leading=true, f)

    debounced.f()
    debounced.f()
    debounced.f()

    Jest.advanceTimersByTime(100)
    let runCount1 = runCount.contents

    debounced.f()
    debounced.f()
    debounced.f()

    Jest.advanceTimersByTime(199)
    let runCount2 = runCount.contents

    Jest.advanceTimersByTime(1)
    let runCount3 = runCount.contents

    expect((runCount1, runCount2, runCount3)) |> toEqual((1, 1, 2))
  })

  test("cancel", () => {
    Jest.useFakeTimers()

    let runCount = ref(0)
    let f = () => runCount := runCount.contents + 1

    let debounced = Debounce.debounce(~delayMS=200, ~leading=false, f)

    let runCount1 = runCount.contents

    debounced.f()
    debounced.f()
    debounced.f()

    Jest.advanceTimersByTime(100)

    let runCount2 = runCount.contents

    debounced.cancel()

    Jest.advanceTimersByTime(300)

    let runCount3 = runCount.contents

    expect((runCount1, runCount2, runCount3)) |> toEqual((0, 0, 0))
  })

  test("flush", () => {
    Jest.useFakeTimers()

    let runCount = ref(0)
    let f = () => runCount := runCount.contents + 1

    let debounced = Debounce.debounce(~delayMS=200, ~leading=false, f)

    let runCount1 = runCount.contents

    debounced.f()
    debounced.f()
    debounced.f()

    Jest.advanceTimersByTime(100)

    let runCount2 = runCount.contents

    debounced.flush()

    let runCount3 = runCount.contents

    Jest.advanceTimersByTime(300)

    let runCount4 = runCount.contents

    expect((runCount1, runCount2, runCount3, runCount4)) |> toEqual((0, 0, 1, 1))
  })
})
