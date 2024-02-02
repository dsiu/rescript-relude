@@uncurried
@@uncurried.swap
// TODO: someday we should abstract the use of Js.Global timeout/interval from these functions, but
// for now, there's not an immediately pressing reason to do so.

@ocaml.doc("
Delays the invocation of a function by [delayMS] milliseconds, and returns a
function to cancel the scheduled call.
")
let delay = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = Js.Global.setTimeout(f, delayMS)
  () => Js.Global.clearTimeout(timerId)
}

@ocaml.doc("
Repeats a function every [delayMS] milliseconds, and returns a function to
cancel the repeat.
")
let repeat = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = Js.Global.setInterval(f, delayMS)
  () => Js.Global.clearInterval(timerId)
}

@ocaml.doc("
Repeats a function every [delayMS] milliseconds, up to [times] times, and
returns a function to cancel the repeat.
")
let repeatTimes = (~delayMS: int, ~times: int, f: unit => unit): (unit => unit) => {
  let timerId = ref(None)
  let cancel = () => timerId.contents->(Relude_Option.forEach(x => Js.Global.clearInterval(x), _))
  let callCount = ref(0)
  timerId := Some(Js.Global.setInterval(() => {
        f()
        callCount := callCount.contents + 1
        if callCount.contents == times {
          cancel()
        }
      }, delayMS))
  cancel
}
