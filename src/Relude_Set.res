@@uncurried
@@uncurried.swap

type t<'value, 'id> = Belt.Set.t<'value, 'id>

@ocaml.doc("
[Set.empty] constructs a new, empty set given an identity module.
")
let empty: module(Belt.Id.Comparable with type t = 'value and type identity = 'id) => Belt.Set.t<
  'value,
  'id,
> = Belt.Set.make(~id=_)

@ocaml.doc("
[Set.singleton] constructs a new set from a single value
")
let singleton: (
  module(Belt.Id.Comparable with type t = 'value and type identity = 'id),
  'value,
) => Belt.Set.t<'value, 'id> = (id, value) => Belt.Set.make(~id)->Belt.Set.add(value)

@ocaml.doc("
[Set.fromArray] converts an array of values into a new set of those values,
ordered by the comparator function from the provided [Comparable] module.
")
let fromArray: (
  module(Belt.Id.Comparable with type t = 'value and type identity = 'id),
  array<'value>,
) => Belt.Set.t<'value, 'id> = (id, value) => Belt.Set.fromArray(~id, value)

@ocaml.doc("
[Set.fromList] converts a list of values into a new set of those values,
ordered by the comparator function [Comparable.cmp].
")
let fromList: (
  module(Belt.Id.Comparable with type t = 'value and type identity = 'id),
  list<'value>,
) => Belt.Set.t<'value, 'id> = (id, value) => Belt.Set.fromArray(~id, Belt.List.toArray(value))

@ocaml.doc("
Determine whether a set is empty.
")
let isEmpty: t<'value, 'id> => bool = s => s->Belt.Set.isEmpty

@ocaml.doc("
Determine whether a set contains a given value.
")
let contains: ('value, t<'value, 'id>) => bool = (value, set) => Belt.Set.has(set, value)

@ocaml.doc("
Immutably add a value to a set. If the value already exists in the set, the
original set (physical reference) is returned unchanged. Otherwise, a new copy
of the set is returned containing the value.
")
let add: ('value, t<'value, 'id>) => t<'value, 'id> = (value, set) => Belt.Set.add(set, value)

@ocaml.doc("
Immutably merge an array of values into a set.

Note: unlike {!val:add} if the set already contains all the values in the array,
the return value may or may not be a new reference. Otherwise, a new set
reference is returned.
")
let mergeMany: (array<'value>, t<'value, 'id>) => t<'value, 'id> = (value, set) =>
  Belt.Set.mergeMany(set, value)

@ocaml.doc("
Immutably remove a value from a set. If the value is not found in the set, the
original set (physical reference) is returned unchanged. Otherwise, a new set is
returned containing the value.
")
let remove: ('value, t<'value, 'id>) => t<'value, 'id> = (value, set) => Belt.Set.remove(set, value)

@ocaml.doc("
Immutably remove multiple values from a set. Note: unlike [remove], if none of
the values in the array are found in the set, the return value may or may not be
a new reference. Otherwise, a new set reference is returned.
")
let removeMany: (array<'value>, t<'value, 'id>) => t<'value, 'id> = (value, set) =>
  Belt.Set.removeMany(set, value)

@ocaml.doc("
Immutably update a set with a new value. If an equivalent value already exists
in the set, it will be immutably removed before adding the new value. In ether
case, a new physical reference to the set will be returned.

Note: unlike {!val:add}, [update] guarantees that the value's physical
reference will be added to the set.
")
let update: ('value, t<'value, 'id>) => t<'value, 'id> = (value, set) =>
  Belt.Set.add(Belt.Set.remove(set, value), value)

@ocaml.doc("
Immutably add a value to a set if it does not already exist. If an equivalent
value already exists in the set, it will be removed. In either case, a new
physical reference to the set will be returned.
")
let toggle: ('value, t<'value, 'id>) => t<'value, 'id> = (value, set) =>
  Belt.Set.has(set, value) ? Belt.Set.remove(set, value) : Belt.Set.add(set, value)

@ocaml.doc("
Returns a new set representing the union of two sets.

{[
  let s1 = fromList([1, 3, 4, 5, 2, 1]);
  let s2 = fromList([1, 0, 3, 2, 9]);
  Set.union(s1, s2) |> toList == [0, 1, 2, 3, 4, 5, 9];
]}
")
let union: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id> = (a, b) => Belt.Set.union(a, b)

@ocaml.doc("
Returns a new set representing the intersection of two sets.

{[
  let s1 = fromList([0, 1, 2, 3, 4, 1]);
  let s2 = fromList([0, 3, 9, 8, 10]);
  Set.intersect(s1, s2) |> toList == [0, 3];
]}
")
let intersect: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id> = (a, b) =>
  Belt.Set.intersect(a, b)

@ocaml.doc("
Returns a new set which contains all the elements of the first set that are not
present in the second set.

Note: The argument order is significant for this function.

{[
  let s1 = fromList([0, 1, 2, 3, 4, 1]);
  let s2 = fromList([0, 3, 9, 8, 10]);

  diff(s1, s2) |> toList == [1, 2, 4];
  diff(s2, s1) |> toList == [8, 9, 10];
]}
")
let diff: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id> = (a, b) => Belt.Set.diff(a, b)

@ocaml.doc("
[subset(s1, s2)] will return [true] if [s2] is a subset of [s1].

Note: The argument order is significant for this function.
")
let subset: (t<'value, 'id>, t<'value, 'id>) => bool = (a, b) => Belt.Set.subset(a, b)

@ocaml.doc("
Returns an integer value of [-1 | 0 | 1], representing the total ordering
between two sets. This can be used as a comparator function to determine the
ordering of nested sets (e.g. [Set.t(Set.t('a))]);

Note: The argument order is significant for this function.

{[
  let s0 = Test1.fromList([1, 2, 3, 4]);
  let s1 = Test1.fromList([1, 2, 3, 4]);
  let s2 = Test1.fromList([2, 3, 4, 5, 6]);
  let s3 = Test1.fromList([100, 0]);

  Set.compare(s0, s1) == 0;
  Set.compare(s0, s2) == -1;
  Set.compare(s0, s3) == 1;

  Set.compare(s1, s2) == -1;
  Set.compare(s1, s3) == 1;
  Set.compare(s2, s3) == 1;
]}
")
let compare: (t<'value, 'id>, t<'value, 'id>) => int = (a, b) => Belt.Set.cmp(a, b)

@ocaml.doc("
Determine whether two sets are equivalent.
")
let eq: (t<'value, 'id>, t<'value, 'id>) => bool = (a, b) => Belt.Set.eq(a, b)

@ocaml.doc("
Apply a function to each element of a set, in increasing order.
")
let forEach: ('value => unit, t<'value, 'id>) => unit = (fn, set) => Belt.Set.forEach(set, fn)

@ocaml.doc("
Iterate over the values of a set in increasing order, accumulating a final value
of an arbitraty type.
")
let foldLeft: (('acc, 'value) => 'acc, 'acc, t<'value, 'id>) => 'acc = (fn, acc, set) =>
  Belt.Set.reduce(set, acc, fn)

@ocaml.doc("
TODO: optimize foldRight for sets. This remains unimplemented in [Belt]'s API,
but it exists in PureScript/Haskell, since [Set] implements [Foldable].
")
let foldRight: (('b, 'a) => 'a, 'a, t<'b, 'id>) => 'a = (fn, acc, set) =>
  Array.fold_right(fn, Belt.Set.toArray(set), acc)

@ocaml.doc("
Determine whether a given predicate holds true for all values in a given set.
")
let all: ('value => bool, t<'value, 'id>) => bool = (predicate, set) =>
  Belt.Set.every(set, predicate)

@ocaml.doc("
Determine whether a given predicate holds true for at least one value in a given
set.
")
let any: ('value => bool, t<'value, 'id>) => bool = (predicate, set) =>
  Belt.Set.some(set, predicate)

@ocaml.doc("
Create a new set from another set containing only the values which pass a given
test (the predicate function).
")
let filter: ('value => bool, t<'value, 'id>) => t<'value, 'id> = (predicate, set) =>
  Belt.Set.keep(set, predicate)

@ocaml.doc("
[Set.keep] is an alias for {!val:filter}.
")
let keep: ('value => bool, t<'value, 'id>) => t<'value, 'id> = filter

@ocaml.doc("
Creates a new set from another set containing only the values which do {e not}
pass a given test (the predicate function)
")
let filterNot: ('value => bool, t<'value, 'id>) => t<'value, 'id> = (predicate, set) =>
  Belt.Set.keep(set, x => !predicate(x))

@ocaml.doc("
[Set.reject] is an alias for {!val.filterNot}.
")
let reject: ('value => bool, t<'value, 'id>) => t<'value, 'id> = filterNot

@ocaml.doc("
Immutably divide a set into a tuple of two sets, where the first set contains
all the values which pass the predicate function test, and the second one
contains all the values which fail.
")
let partition: ('value => bool, t<'value, 'id>) => (t<'value, 'id>, t<'value, 'id>) = (
  predicate,
  set,
) => Belt.Set.partition(set, predicate)

@ocaml.doc("
Returns the total number of elements in a set.
")
let length: t<'value, 'id> => int = s => s->Belt.Set.size

@ocaml.doc("
Creates a new array containing all elements of the set in ascending order based
on the associated comparator function.
")
let toArray: t<'value, 'id> => array<'value> = s => s->Belt.Set.toArray

@ocaml.doc("
Creates a new list containing all elements of the set in ascending order based
on the associated comparator function.
")
let toList: t<'value, 'id> => list<'value> = s => s->Belt.Set.toList

@ocaml.doc("
Optionally returns the lowest ordered element in a given set or [None] if the
set is empty.
")
let minimum: t<'value, 'id> => option<'value> = s => s->Belt.Set.minimum

@ocaml.doc("
Optionally returns the highest ordered element in a given set or [None] if the
set is empty.
")
let maximum: t<'value, 'id> => option<'value> = s => s->Belt.Set.maximum

@ocaml.doc("
Optionally returns an equivalent element from a set or [None] if no equivalent
element is found, or the set is empty.
")
let get: ('value, t<'value, 'id>) => option<'value> = (value, set) => Belt.Set.get(set, value)

@ocaml.doc("
Returns an equivalent element from a set if one exists, or else returns a
specified default value.
")
let getOrElse: ('value, 'value, t<'value, 'id>) => 'value = (value, default, set) =>
  Relude_Option_Base.getOrElse(default, Belt.Set.get(set, value))

@ocaml.doc("
TODO: Needs documentation. Belt doesn't provide much description.
")
let split: ('value, t<'value, 'id>) => ((t<'value, 'id>, t<'value, 'id>), bool) = (value, set) =>
  Belt.Set.split(set, value)

module type SET = {
  module Comparable: {
    type identity
    type t
  }

  type value
  type t = Belt.Set.t<value, Comparable.identity>
  let empty: t
  let singleton: value => t
  let fromArray: array<value> => t
  let fromList: list<value> => t
  let isEmpty: t => bool
  let contains: (value, t) => bool
  let add: (value, t) => t
  let mergeMany: (array<value>, t) => t
  let remove: (value, t) => t
  let removeMany: (array<value>, t) => t
  let update: (value, t) => t
  let toggle: (value, t) => t
  let union: (t, t) => t
  let intersect: (t, t) => t
  let diff: (t, t) => t
  let subset: (t, t) => bool
  let compare: (t, t) => int
  let eq: (t, t) => bool
  let forEach: (value => unit, t) => unit
  let foldLeft: (('a, value) => 'a, 'a, t) => 'a
  let foldRight: ((value, 'a) => 'a, 'a, t) => 'a
  let all: (value => bool, t) => bool
  let any: (value => bool, t) => bool
  let filter: (value => bool, t) => t
  let keep: (value => bool, t) => t
  let filterNot: (value => bool, t) => t
  let reject: (value => bool, t) => t
  let partition: (value => bool, t) => (t, t)
  let length: t => int
  let toArray: t => array<value>
  let toList: t => list<value>
  let minimum: t => option<value>
  let maximum: t => option<value>
  let get: (value, t) => option<value>
  let getOrElse: (value, value, t) => value
  let split: (value, t) => ((t, t), bool)
}

module WithOrd = (M: BsBastet.Interface.ORD): (
  SET with type value = M.t and type Comparable.t = M.t
) => {
  type value = M.t

  module Comparable = Belt.Id.MakeComparable({
    type t = value
    let cmp = (. a, b) => Relude_Ordering.toInt(M.compare(a, b))
  })

  type t = t<value, Comparable.identity>

  let empty: t = empty(module(Comparable))
  let singleton: value => t = singleton(module(Comparable), ...)
  let fromArray: array<value> => t = fromArray(module(Comparable), ...)
  let fromList: list<value> => t = fromList(module(Comparable), ...)
  let isEmpty: t => bool = isEmpty
  let contains: (value, t) => bool = contains
  let add: (value, t) => t = add
  let mergeMany: (array<value>, t) => t = mergeMany
  let remove: (value, t) => t = remove
  let removeMany: (array<value>, t) => t = removeMany
  let update: (value, t) => t = update
  let toggle: (value, t) => t = toggle
  let union: (t, t) => t = union
  let intersect: (t, t) => t = intersect
  let diff: (t, t) => t = diff
  let subset: (t, t) => bool = subset
  let compare: (t, t) => int = compare
  let eq: (t, t) => bool = eq
  let forEach: (value => unit, t) => unit = forEach
  let foldLeft: (('acc, value) => 'acc, 'acc, t) => 'acc = foldLeft
  let foldRight: ((value, 'a) => 'a, 'a, t) => 'a = foldRight
  let all: (value => bool, t) => bool = all
  let any: (value => bool, t) => bool = any
  let filter: (value => bool, t) => t = filter
  let keep: (value => bool, t) => t = filter
  let filterNot: (value => bool, t) => t = filterNot
  let reject: (value => bool, t) => t = filterNot
  let partition: (value => bool, t) => (t, t) = partition
  let length: t => int = length
  let toArray: t => array<value> = toArray
  let toList: t => list<value> = toList
  let minimum: t => option<value> = minimum
  let maximum: t => option<value> = maximum
  let get: (value, t) => option<value> = get
  let getOrElse: (value, value, t) => value = getOrElse
  let split: (value, t) => ((t, t), bool) = split
}
