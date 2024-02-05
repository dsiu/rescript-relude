@@uncurried
@@uncurried.swap

// TODO: someday we should abstract away the use of Js.Global timeout from this

@ocaml.doc("
The data returned from the throttle function.
")
type throttled = {
  cancel: unit => unit,
  isThrottled: unit => bool,
  f: unit => unit,
}

@ocaml.doc("
The throttle function takes a unit => unit function and returns a new function
(and other control values) which when used, suppresses calls to the given
function to only once within the given delayMS.

This is useful for sampling a stream of function calls on some interval.

If [leading] is given as true, the function will be allowed to run on the first
call before the throttling behavior kicks in.
")
let throttle = (~delayMS: int, ~leading: bool=false, f: unit => unit): throttled => {
  let isThrottled = ref(false)
  let timerId = ref(None)

  let cancel = () => {
    timerId.contents->(Relude_Option.forEach(timerId => Js.Global.clearTimeout(timerId), _))
    timerId := None
  }

  let attempt = () =>
    if isThrottled.contents {
      ()
    } else {
      cancel()
      isThrottled := true
      f()
      timerId := Some(Js.Global.setTimeout(() => {
            isThrottled := false
            timerId := None
          }, delayMS))
    }

  let isThrottled = () => isThrottled.contents

  let doLeading = ref(leading)

  let throttled = () =>
    if doLeading.contents {
      doLeading := false
      f()
    } else {
      attempt()
    }

  {f: throttled, cancel, isThrottled}
}
