open Jest
open Expect
open! Relude.Globals

module Month = {
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

  let toInt1Based: t => int = x =>
    switch x {
    | Jan => 1
    | Feb => 2
    | Mar => 3
    | Apr => 4
    | May => 5
    | Jun => 6
    | Jul => 7
    | Aug => 8
    | Sep => 9
    | Oct => 10
    | Nov => 11
    | Dec => 12
    }

  let fromInt1Based: int => option<t> = x =>
    switch x {
    | 1 => Some(Jan)
    | 2 => Some(Feb)
    | 3 => Some(Mar)
    | 4 => Some(Apr)
    | 5 => Some(May)
    | 6 => Some(Jun)
    | 7 => Some(Jul)
    | 8 => Some(Aug)
    | 9 => Some(Sep)
    | 10 => Some(Oct)
    | 11 => Some(Nov)
    | 12 => Some(Dec)
    | _ => None
    }

  module Eq: BsBastet.Interface.EQ with type t = t = Int.EqBy({
    type a = t
    type b = int
    let f = toInt1Based
  })

  module Ord: BsBastet.Interface.ORD with type t = t = Int.OrdBy({
    type a = t
    type b = int
    let f = toInt1Based
  })

  module Bounded: BsBastet.Interface.BOUNDED with type t = t = {
    include Ord
    let bottom = Jan
    let top = Dec
  }

  module Enum: Relude.Interface.ENUM with type t = t = {
    include Ord
    let pred: t => option<t> = month => fromInt1Based(toInt1Based(month) - 1)

    let succ: t => option<t> = month => fromInt1Based(toInt1Based(month) + 1)
  }

  module BoundedEnum: Relude.Interface.BOUNDED_ENUM with type t = t = {
    include Bounded
    include (Enum: Relude.Interface.ENUM with type t := t)
    let cardinality = 12
    let toEnum = fromInt1Based
    let fromEnum = toInt1Based
  }

  module Show: BsBastet.Interface.SHOW with type t = t = {
    type t = t
    let show: t => string = x =>
      switch x {
      | Jan => "Jan"
      | Feb => "Feb"
      | Mar => "Mar"
      | Apr => "Apr"
      | May => "May"
      | Jun => "Jun"
      | Jul => "Jul"
      | Aug => "Aug"
      | Sep => "Sep"
      | Oct => "Oct"
      | Nov => "Nov"
      | Dec => "Dec"
      }
  }
  include Relude.Extensions.Eq.EqExtensions(Eq)
  include Relude.Extensions.Bounded.BoundedExtensions(Bounded)
  include Relude.Extensions.Enum.EnumExtensions(Enum)
  include Relude.Extensions.BoundedEnum.BoundedEnumExtensions(BoundedEnum)
  include Relude.Extensions.Show.ShowExtensions(Show)
}

open Month

describe("Relude_Extensions_Enum", () => {
  testAll(
    "fromToAsList",
    list{
      (Jan, Dec, list{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Jan, Jan, list{Jan}),
      (Jan, Feb, list{Jan, Feb}),
      (Jan, Mar, list{Jan, Feb, Mar}),
      (Jan, Nov, list{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov}),
      (Feb, Nov, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov}),
      (Feb, Dec, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Dec, Jan, list{Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Dec, Feb, list{Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb}),
      (Nov, Jan, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Nov, Feb, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb}),
      (Dec, Nov, list{Dec, Nov}),
      (Dec, Dec, list{Dec}),
    },
    ((start, finish, expected)) => {
      let actual = Month.fromToAsList(~start, ~finish)
      expect(actual) -> toEqual(expected)
    },
  )

  testAll(
    "upFromAsList",
    list{
      (Jan, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Feb, list{Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Nov, list{Dec}),
      (Dec, list{}),
    },
    ((start, expected)) => {
      let actual = Month.upFromAsList(start)
      expect(actual) -> toEqual(expected)
    },
  )

  testAll(
    "upFromIncludingAsList",
    list{
      (Jan, list{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Feb, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Nov, list{Nov, Dec}),
      (Dec, list{Dec}),
    },
    ((start, expected)) => {
      let actual = Month.upFromIncludingAsList(start)
      expect(actual) -> toEqual(expected)
    },
  )

  testAll(
    "downFromAsList",
    list{
      (Jan, list{}),
      (Feb, list{Jan}),
      (Nov, list{Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Dec, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
    },
    ((start, expected)) => {
      let actual = Month.downFromAsList(start)
      expect(actual) -> toEqual(expected)
    },
  )

  testAll(
    "downFromIncludingAsList",
    list{
      (Jan, list{Jan}),
      (Feb, list{Feb, Jan}),
      (Nov, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Dec, list{Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
    },
    ((start, expected)) => {
      let actual = Month.downFromIncludingAsList(start)
      expect(actual) -> toEqual(expected)
    },
  )
})

describe("Relude_Extensions_BoundedEnum", () => {
  testAll(
    "fromThenToAsList",
    list{
      (Jan, Feb, Dec, list{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Jan, Feb, Jan, list{Jan}),
      (Jan, Feb, Feb, list{Jan, Feb}),
      (Jan, Feb, Mar, list{Jan, Feb, Mar}),
      (Jan, Feb, Nov, list{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov}),
      (Feb, Mar, Nov, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov}),
      (Feb, Mar, Dec, list{Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}),
      (Dec, Nov, Jan, list{Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Dec, Nov, Feb, list{Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb}),
      (Nov, Oct, Jan, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan}),
      (Nov, Oct, Feb, list{Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb}),
      (Dec, Nov, Nov, list{Dec, Nov}),
      (Dec, Nov, Dec, list{Dec}),
      (Jan, Mar, Dec, list{Jan, Mar, May, Jul, Sep, Nov}),
      (Jan, Jun, Dec, list{Jan, Jun, Nov}),
      (Jan, Oct, Dec, list{Jan, Oct}),
      (Jan, Dec, Dec, list{Jan, Dec}),
      (Dec, Oct, Jan, list{Dec, Oct, Aug, Jun, Apr, Feb}),
      (Dec, Feb, Jan, list{Dec, Feb}),
      (Dec, Jan, Jan, list{Dec, Jan}),
    },
    ((start, next, finish, expected)) => {
      let actual = Month.fromThenToAsList(~start, ~next, ~finish)
      expect(actual) -> toEqual(expected)
    },
  )
  describe("Relude_Extensions_BoundedEnum", () => {
    let show = Show.show
    let parseOrd = inverseMapOrd(~ordA=module(Relude_String.Ord), show)
    let parseOrdBy = inverseMapOrdBy(Relude.String.compare, show)
    let parseEq = inverseMapEq(~eqA=module(Relude_String.Eq), show)
    let parseEqBy = inverseMapEqBy(Relude.String.eq, show)
    let parseString = inverseMapString(show)

    testAll(
      "inverseMapOrd",
      list{
        ("Jan", Some(Jan)),
        ("Feb", Some(Feb)),
        ("Mar", Some(Mar)),
        ("Apr", Some(Apr)),
        ("May", Some(May)),
        ("Jun", Some(Jun)),
        ("Jul", Some(Jul)),
        ("Aug", Some(Aug)),
        ("Sep", Some(Sep)),
        ("Oct", Some(Oct)),
        ("Nov", Some(Nov)),
        ("Dec", Some(Dec)),
        ("", None),
        ("jan", None),
        ("feb", None),
        ("mar", None),
        ("apr", None),
        ("may", None),
        ("jun", None),
        ("jul", None),
        ("aug", None),
        ("sep", None),
        ("oct", None),
        ("nov", None),
        ("dec", None),
      },
      ((string, expected)) => {
        let actual = parseOrd(string)
        expect(actual) -> toEqual(expected)
      },
    )

    testAll(
      "inverseMapOrdBy",
      list{
        ("Jan", Some(Jan)),
        ("Feb", Some(Feb)),
        ("Mar", Some(Mar)),
        ("Apr", Some(Apr)),
        ("May", Some(May)),
        ("Jun", Some(Jun)),
        ("Jul", Some(Jul)),
        ("Aug", Some(Aug)),
        ("Sep", Some(Sep)),
        ("Oct", Some(Oct)),
        ("Nov", Some(Nov)),
        ("Dec", Some(Dec)),
        ("", None),
        ("jan", None),
        ("feb", None),
        ("mar", None),
        ("apr", None),
        ("may", None),
        ("jun", None),
        ("jul", None),
        ("aug", None),
        ("sep", None),
        ("oct", None),
        ("nov", None),
        ("dec", None),
      },
      ((string, expected)) => {
        let actual = parseOrdBy(string)
        expect(actual) -> toEqual(expected)
      },
    )

    testAll(
      "inverseMap all tests",
      list{
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec",
        "",
        "jan",
        "feb",
        "mar",
        "apr",
        "may",
        "jun",
        "jul",
        "aug",
        "sep",
        "oct",
        "nov",
        "dec",
      },
      string => {
        let usingInverseMapOrd = parseOrd(string)
        let usingInverseMapOrdBy = parseOrdBy(string)
        let usingInverseMapEq = parseEq(string)
        let usingInverseMapEqBy = parseEqBy(string)
        let usingInverseMapString = parseString(string)
        expect((
          usingInverseMapOrd,
          usingInverseMapOrdBy,
          usingInverseMapEq,
          usingInverseMapEqBy,
          usingInverseMapString,
        )) -> toEqual((
          usingInverseMapOrdBy,
          usingInverseMapEq,
          usingInverseMapEqBy,
          usingInverseMapString,
          usingInverseMapOrd,
        ))
      },
    )
  })
})
