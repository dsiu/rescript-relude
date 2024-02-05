@@uncurried
@@uncurried.swap

type t<'a, 'b> =
  | This('a)
  | That('b)
  | Both('a, 'b)
