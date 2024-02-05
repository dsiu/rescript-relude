@@uncurried
@@uncurried.swap

open Jest
open Expect

module HMap = Relude.HMap

// Test HMap with key info - a string identifier, and a function to convert the values to a string
// This type of key info is necessary for things like folding (I think), otherwise you have no way to know
// what type you're dealing with (because 'a is existential in KeyValue(keyData('a), 'a))
module HMapS = HMap.WithKeyMeta({
  // Our key info is a tuple of a label, and a function to convert the value into a string
  type t<'a> = (string, 'a => string)
})

describe("HMap", () => {
  test("empty has size 0", () => expect(HMap.empty->HMap.size)->toEqual(0))

  test("singleton", () => {
    let intKey = HMap.Key.create()
    let map = HMap.singleton(intKey, 42)
    expect((map->(HMap.find(intKey, _)), map->HMap.size))->toEqual((Some(42), 1))
  })

  test("add, hasKey, find, size", () => {
    let intKey = HMap.Key.create()
    let stringKey = HMap.Key.create()

    let map = HMap.empty->HMap.add(intKey, 5, _)->(HMap.add(stringKey, "hi", _))

    expect((
      map->(HMap.hasKey(intKey, _)),
      map->(HMap.find(intKey, _)),
      map->(HMap.hasKey(stringKey, _)),
      map->(HMap.find(stringKey, _)),
      map->HMap.size,
    ))->toEqual((true, Some(5), true, Some("hi"), 2))
  })

  test("find success", () => {
    let intKey = HMap.Key.create()
    let map = HMap.empty->(HMap.add(intKey, 5, _))
    expect(HMap.find(intKey, map))->toEqual(Some(5))
  })

  test("find failure", () => {
    let intKey = HMap.Key.create()
    let stringKey = HMap.Key.create()
    let map = HMap.empty->(HMap.add(intKey, 5, _))
    expect(HMap.find(stringKey, map))->toEqual(None)
  })

  test("remove", () => {
    let intKey1 = HMap.Key.create()
    let intKey2 = HMap.Key.create()
    let intKey3 = HMap.Key.create()
    let map123 =
      HMap.empty->HMap.add(intKey1, 1, _)->HMap.add(intKey2, 2, _)->(HMap.add(intKey3, 3, _))
    let map13 = map123->(HMap.remove(intKey2, _))
    let map1 = map13->(HMap.remove(intKey3, _))
    let map = map1->(HMap.remove(intKey1, _))
    expect((
      map123->HMap.size,
      map123->(HMap.find(intKey1, _)),
      map123->(HMap.find(intKey2, _)),
      map123->(HMap.find(intKey3, _)),
      map13->HMap.size,
      map13->(HMap.find(intKey1, _)),
      map13->(HMap.find(intKey2, _)),
      map13->(HMap.find(intKey3, _)),
      map1->HMap.size,
      map1->(HMap.find(intKey1, _)),
      map1->(HMap.find(intKey2, _)),
      map1->(HMap.find(intKey3, _)),
      map->HMap.size,
      map->(HMap.find(intKey1, _)),
      map->(HMap.find(intKey2, _)),
      map->(HMap.find(intKey3, _)),
    ))->toEqual((
      3,
      Some(1),
      Some(2),
      Some(3),
      2,
      Some(1),
      None,
      Some(3),
      1,
      Some(1),
      None,
      None,
      0,
      None,
      None,
      None,
    ))
  })

  test("fold", () => {
    // In order to iterate over the map, we need a function that knows how to convert the existential
    // type 'a into a value we can use.  Here we encode an extra label with our key, and a function to convert
    // each value into a string.
    let intKey = HMapS.Key.create(("key1", string_of_int))
    let stringKey = HMapS.Key.create(("key2", a => a))
    let floatKey = HMapS.Key.create(("key3", x => Js.Float.toString(x)))

    let map =
      HMapS.empty
      ->HMapS.add(intKey, 5, _)
      ->HMapS.add(stringKey, "hi", _)
      ->(HMapS.add(floatKey, 42.3, _))

    // Use keyMeta to extract the key info - in this case our label, and the 'a => string function
    // that got embedded in the key
    let value = map->(
      HMapS.fold(
        (HMapS.KeyValue(k, v), acc) => {
          let (label, show) = HMapS.Key.keyMeta(k)
          list{label, show(v), ...acc}
        },
        list{},
        _,
      )
    )

    expect(value)->toEqual(list{"key3", "42.3", "key2", "hi", "key1", "5"})
  })

  test("forEach", () => {
    // In order to iterate over the map, we need a function that knows how to convert the existential
    // type 'a into a value we can use.  Here we encode an extra label with our key, and a function to convert
    // each value into a string.
    let intKey = HMapS.Key.create(("key1", string_of_int))
    let stringKey = HMapS.Key.create(("key2", a => a))
    let floatKey = HMapS.Key.create(("key3", x => Js.Float.toString(x)))

    let map =
      HMapS.empty
      ->HMapS.add(intKey, 5, _)
      ->HMapS.add(stringKey, "hi", _)
      ->(HMapS.add(floatKey, 42.3, _))

    let array = []

    map->(
      HMapS.forEach(
        (HMapS.KeyValue(k, v)) => {
          let (label, show) = HMapS.Key.keyMeta(k)
          array->Js.Array.unshift(show(v), _)->ignore
          array->Js.Array.unshift(label, _)->ignore
        },
        _,
      )
    )

    expect(array)->toEqual(["key3", "42.3", "key2", "hi", "key1", "5"])
  })

  test("all", () => {
    // In order to iterate over the map, we need a function that knows how to convert the existential
    // type 'a into a value we can use.  Here we encode an extra label with our key, and a function to convert
    // each value into a string.
    let intKey = HMapS.Key.create(("key1", string_of_int))
    let stringKey = HMapS.Key.create(("key2", a => a))
    let floatKey = HMapS.Key.create(("key3", x => Js.Float.toString(x)))

    let map =
      HMapS.empty
      ->HMapS.add(intKey, 5, _)
      ->HMapS.add(stringKey, "hi", _)
      ->(HMapS.add(floatKey, 42.3, _))

    // Use keyMeta to extract the key info - in this case our label, and the 'a => string function
    // that got embedded in the key
    let value = map->(
      HMapS.all(
        (HMapS.KeyValue(k, _)) => {
          let (label, _) = HMapS.Key.keyMeta(k)
          Relude.String.contains(~search="key", label)
        },
        _,
      )
    )

    expect(value)->toEqual(true)
  })

  test("any", () => {
    // In order to iterate over the map, we need a function that knows how to convert the existential
    // type 'a into a value we can use.  Here we encode an extra label with our key, and a function to convert
    // each value into a string.
    let intKey = HMapS.Key.create(("key1", string_of_int))
    let stringKey = HMapS.Key.create(("key2", a => a))
    let floatKey = HMapS.Key.create(("key3", x => Js.Float.toString(x)))

    let map =
      HMapS.empty
      ->HMapS.add(intKey, 5, _)
      ->HMapS.add(stringKey, "hi", _)
      ->(HMapS.add(floatKey, 42.3, _))

    let value = map->(
      HMapS.any(
        (HMapS.KeyValue(k, _)) => {
          let (label, _) = HMapS.Key.keyMeta(k)
          Relude.String.contains(~search="key2", label)
        },
        _,
      )
    )

    expect(value)->toEqual(true)
  })

  test("filter", () => {
    // In order to iterate over the map, we need a function that knows how to convert the existential
    // type 'a into a value we can use.  Here we encode an extra label with our key, and a function to convert
    // each value into a string.
    let intKey = HMapS.Key.create(("key1", string_of_int))
    let stringKey = HMapS.Key.create(("key2", a => a))
    let floatKey = HMapS.Key.create(("key3", x => Js.Float.toString(x)))

    let map =
      HMapS.empty
      ->HMapS.add(intKey, 5, _)
      ->HMapS.add(stringKey, "hi", _)
      ->(HMapS.add(floatKey, 42.3, _))

    let map2 = map->(
      HMapS.filter(
        (HMapS.KeyValue(k, _)) => {
          let (label, _) = HMapS.Key.keyMeta(k)
          label != "key2"
        },
        _,
      )
    )

    expect(map2->HMapS.size)->toEqual(2)
  })
})
