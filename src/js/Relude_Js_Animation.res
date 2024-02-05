@@uncurried
@@uncurried.swap

type rafId

@val
external requestAnimationFrame: (float => unit) => rafId = "requestAnimationFrame"

@val
external cancelAnimationFrame: rafId => unit = "cancelAnimationFrame"
