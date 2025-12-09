@@uncurried
@@uncurried.swap
// TODO: someday we should abstract the use of Js.Global timeout/interval from these functions, but
// for now, there's not an immediately pressing reason to do so.

@ocaml.doc("
Delays the invocation of a function by [delayMS] milliseconds, and returns a
function to cancel the scheduled call.
")
let delay = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = setTimeout(f, delayMS)
  () => clearTimeout(timerId)
}

@ocaml.doc("
Repeats a function every [delayMS] milliseconds, and returns a function to
cancel the repeat.
")
let repeat = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = setInterval(f, delayMS)
  () => clearInterval(timerId)
}

@ocaml.doc("
Repeats a function every [delayMS] milliseconds, up to [times] times, and
returns a function to cancel the repeat.
")
let repeatTimes = (~delayMS: int, ~times: int, f: unit => unit): (unit => unit) => {
  let timerId = ref(None)
  let cancel = () => timerId.contents->(Relude_Option.forEach(x => clearInterval(x), _))
  let callCount = ref(0)
  timerId := Some(setInterval(() => {
        f()
        callCount := callCount.contents + 1
        if callCount.contents == times {
          cancel()
        }
      }, delayMS))
  cancel
}
