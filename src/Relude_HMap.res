@@uncurried
@@uncurried.swap

// Inspired by https://github.com/dbuenzli/hmap

@ocaml.doc("
Witness contains a type [t] with an existential type variable - a type that we
capture, but lose knowledge of once captured.

[t(_)] is an extensible variant type, indicated by the [..]. This is used below
in WITNESS so that we can add new cases to the Witness.t(_) type on demand, so
that we can create witnesses for any new types we encounter going forward.
")
module Witness = {
  type t<_> = ..
}

@ocaml.doc("
[WITNESS] captures a type [t] with a [Witness] module for that type. Because
[Witness.t(_)] is an extensible variant, we use [+=] to add a constructor
to that type.

This is useful because the type [t] is unified with the type [t] hidden inside
the Witness module.
")
module type WITNESS = {
  type t
  type Witness.t<_> +=
    | Witness: Witness.t<t>
}

@ocaml.doc("
[witness('a)] is a type which captures a WITNESS module for the given type ['a].
")
type witness<'a> = module(WITNESS with type t = 'a)

@ocaml.doc("
[makeWitness] makes a Witness module for the given type [a]
")
let makeWitness = (type a, ()): module(WITNESS with type t = a) => {
  module Witness = {
    type t = a
    type Witness.t<_> +=
      | Witness: Witness.t<t>
  }
  module(Witness: WITNESS with type t = a)
}

@ocaml.doc("
[typeEq('a, 'b)] contains a single constructor TypeEq which can only be
constructed if ['a] and ['b] are the same types.

This is used for a type-level type equality check.
")
type rec typeEq<'a, 'b> = TypeEq: typeEq<'a, 'a>

@ocaml.doc("
[typeEq] checks whether the give witness types are equal.
")
let typeEq:
  type l r. (witness<l>, witness<r>) => option<typeEq<l, r>> =
  (l, r) => {
    module L = unpack(l: WITNESS with type t = l)
    module R = unpack(r: WITNESS with type t = r)
    switch L.Witness {
    | R.Witness => Some(TypeEq)
    | _ => None
    }
  }

@ocaml.doc("
[KEY_META] is a module type signature which captures the type of a map key.
")
module type KEY_META = {
  type t<'a>
}

@ocaml.doc("
[HMAP_TYPE] is a module type signature which captures the types and functions
exposed by the HMap.
")
module type HMAP_TYPE = {
  @ocaml.doc("
  An abstract type for map keys
  ")
  type keyImpl<'a>

  @ocaml.doc("
  Key-related types and operations for an HMap
  ")
  module Key: {
    @ocaml.doc("
    [keyMeta('a)] is the metadata we store with the key, like labels, and other
    functions for operating on the otherwise existential ['a] value.
    ")
    type keyMeta<'a>

    @ocaml.doc("
    Creates a keyImpl (an actual map key) for the given key metadata
    ")
    let create: keyMeta<'a> => keyImpl<'a>

    @ocaml.doc("
    Extracts the key metadata from a keyImpl (an actual map key)
    ")
    let keyMeta: keyImpl<'a> => keyMeta<'a>

    @ocaml.doc("
    The abstract type of our map key
    ")
    type t

    @ocaml.doc("
    Wraps the type-aware keyImpl('a) into the abstract key type
    ")
    let hideType: keyImpl<'a> => t

    @ocaml.doc("
    Checks if two keys are equal
    ")
    let eq: (t, t) => bool

    @ocaml.doc("
    Compares two keys
    ")
    let compare: (. t, t) => int
  }

  @ocaml.doc("
  The abstract type of the HMap
  ")
  type t

  @ocaml.doc("
  An empty HMap
  ")
  let empty: t

  @ocaml.doc("
  Indicates if the HMap is empty
  ")
  let isEmpty: t => bool

  @ocaml.doc("
  Indicates if the HMap has a value for the given key
  ")
  let hasKey: (keyImpl<'a>, t) => bool

  @ocaml.doc("
  Adds the given key/value pair to the HMap
  ")
  let add: (keyImpl<'a>, 'a, t) => t

  @ocaml.doc("
  Creates an HMap with the given key/value pair
  ")
  let singleton: (keyImpl<'a>, 'a) => t

  @ocaml.doc("
  Creates a new HMap that does not contain a value for the given key
  ")
  let remove: (keyImpl<'a>, t) => t

  @ocaml.doc("
  Looks up a value in the HMap for the given key
  ")
  let find: (keyImpl<'a>, t) => option<'a>

  @ocaml.doc("
  The type of a key/value pair in the HMap. The key captures the type of the
  corresponding value.
  ")
  type rec keyValue = KeyValue(keyImpl<'a>, 'a): keyValue

  @ocaml.doc("
  Runs a side effect for each key/value pair in the HMap. Note: the KEY_META
  must provide appropriate functions for converting the existentially typed
  values into values of a known type.
  ")
  let forEach: (keyValue => unit, t) => unit

  @ocaml.doc("
  Folds the HMap into a value. Note the KEY_META must provide appropriate
  functions for manipulating the values stored for each key.
  ")
  let fold: ((keyValue, 'a) => 'a, 'a, t) => 'a

  @ocaml.doc("
  Indicates if all key/value pairs in the HMap satisfy the given predicate.

  Note the KEY_META must provide appropriate functions for manipulating the
  values stored for each key.
  ")
  let all: (keyValue => bool, t) => bool

  @ocaml.doc("
  Indicates if any key/value pairs in the HMap satisfy the given predicate.

  Note the KEY_META must provide appropriate functions for manipulating the
  values stored for each key.
  ")
  let any: (keyValue => bool, t) => bool

  @ocaml.doc("
  Creates a new HMap that only contains the key/value pairs that satisfy the
  given predicate.

  Note the KEY_META must provide appropriate functions for manipulating the
  values stored for each key.
  ")
  let filter: (keyValue => bool, t) => t

  @ocaml.doc("
  Gets the number of key/value pairs stored in this HMap.
  ")
  let size: t => int
}

@ocaml.doc("
Make creates a Map module for the given KEY_META.

KEY_META contains extra information to store with the key, like labels, and functions for
operating on the values contained with each key-value pair.
")
module WithKeyMeta = (KeyMeta: KEY_META): (HMAP_TYPE with type Key.keyMeta<'a> = KeyMeta.t<'a>) => {
  module Key = {
    @ocaml.doc("
    The type of metadata we store with the key
    ")
    type keyMeta<'a> = KeyMeta.t<'a>

    @ocaml.doc("
    keyImpl('a) captures a unique int identifier for the given key type, a witness, and the key meta value
    ")
    type keyImpl<'a> = {
      intId: int,
      witness: witness<'a>,
      keyMeta: keyMeta<'a>,
    }

    @ocaml.doc("
    An impure function that produces monotonically increasing int values.
    ")
    let uniqueInt: unit => int = {
      let id = ref(-1)
      () => {
        incr(id)
        id.contents
      }
    }

    @ocaml.doc("
    Creates a keyImpl('a) value for the given keyMeta('a)

    This is used to create actual keys that we can use to add values to the
    HMap, and retrieve values from the HMap.
    ")
    let create: keyMeta<'a> => keyImpl<'a> = keyMeta => {
      let intId = uniqueInt()
      let witness = makeWitness()
      {intId, witness, keyMeta}
    }

    @ocaml.doc("
    Gets the keyType for a given keyData value
    ")
    let keyMeta: keyImpl<'a> => keyMeta<'a> = keyData => keyData.keyMeta

    @ocaml.doc("
    The abstract Key type containing the existential key type
    ")
    type rec t = Key(keyImpl<'a>): t

    @ocaml.doc("
    hideType wraps the keyData in a structure which hides the key type ['a]
    using an existential.
    ")
    let hideType: keyImpl<'a> => t = k => Key(k)

    @ocaml.doc("
    Compares keys for equality using the unique int ID assigned to the key type
    ")
    let eq: (t, t) => bool = (Key(a), Key(b)) => (compare: (int, int) => int)(a.intId, b.intId) == 0

    @ocaml.doc("
    Compares keys using the unique int ID assigned to the key type
    ")
    let compare: (. t, t) => int = (Key(a), Key(b)) =>
      (compare: (int, int) => int)(a.intId, b.intId)
  }

  type keyImpl<'a> = Key.keyImpl<'a>

  @ocaml.doc("
  Create our HMap with the OCaml Map type, using our Key
  ")
  module HMap = Map.Make(Key)

  @ocaml.doc("
  A type which captures a type-aware key, and the corresponding value. The type
  is existential, so we rely on embedded key metadata to operate on that type if
  needed.
  ")
  type rec keyValue = KeyValue(keyImpl<'a>, 'a): keyValue

  type t = HMap.t<keyValue>

  let empty = HMap.empty

  let isEmpty = m => HMap.is_empty(m)

  let hasKey = (k, m) => HMap.mem(Key.Key(k), m)

  let add = (k, v, m) => HMap.add(Key.Key(k), KeyValue(k, v), m)

  let singleton = (k, v) => HMap.singleton(Key.Key(k), KeyValue(k, v))

  let remove = (k, m) => HMap.remove(Key.Key(k), m)

  let find:
    type a. (keyImpl<a>, t) => option<a> =
    (k, s) =>
      try switch HMap.find(Key.Key(k), s) {
      | KeyValue(k', v) =>
        switch typeEq(k.Key.witness, k'.Key.witness) {
        | None => None
        | Some(TypeEq) => Some(v)
        }
      } catch {
      | Not_found => None
      }

  let forEach = (f: 'a => unit, m: HMap.t<'a>) => HMap.iter((_key, value) => f(value), m)

  let fold = (f: ('a, 'acc) => 'acc, acc: 'acc, m: HMap.t<'a>) =>
    HMap.fold((_key, value, acc) => f(value, acc), m, acc)

  let all = (p: 'a => bool, m: HMap.t<'a>): bool => HMap.for_all((_, b) => p(b), m)

  let any = (p: 'a => bool, m: HMap.t<'a>): bool => HMap.exists((_, b) => p(b), m)

  let filter = (p: 'a => bool, m: HMap.t<'a>): HMap.t<'a> => HMap.filter((_, b) => p(b), m)

  let size: HMap.t<'a> => int = m => HMap.cardinal(m)
}

@ocaml.doc("
We include a default HMap implementation which uses [unit] as it's key meta type.
Note that this default map type will not work in a type-safe way for functions that
iterate over the map with keyValue functions, like fold, all, any, etc.

In order to use the iterating functions, use a custom HMap with the key metadata functions
needed to convert the existential 'a into a useful value.
")
module WithKeyMetaUnit = WithKeyMeta({
  type t<'a> = unit
})
include WithKeyMetaUnit
