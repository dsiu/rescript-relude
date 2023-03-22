open BsBastet.Interface

@ocaml.doc("
[List.cons] prepends the given item to the start of the given list. This
preserves the memory used by the existing list.

Running time: O(1)

{[
  List.cons(\"x\", [\"y\", \"z\"]) == [\"x\", \"y\", \"z\"];
  List.cons(0, []) == [0];
]}
")
let cons: 'a. ('a, list<'a>) => list<'a> = (x, xs) => list{x, ...xs}

@ocaml.doc("
[List.consOption] prepends the given item to the start of the given list when
the item is Some. This preserves the memory used by the existing list.

Running time: O(1)

{[
  List.consOption(Some(\"x\"), [\"y\", \"z\"]) == [\"x\", \"y\", \"z\"];
  List.consOption(None, [\"y\", \"z\"]) == [\"y\", \"z\"];
  List.consOption(Some(0), []) == [0];
]}
")
let consOption: 'a. (option<'a>, list<'a>) => list<'a> = (x, xs) =>
  x |> Relude_Option_Base.fold(xs, x => list{x, ...xs})

@ocaml.doc("
[List.prepend] is an alias for {!val:cons}.
")
let prepend: 'a. ('a, list<'a>) => list<'a> = cons

@ocaml.doc("
[List.uncons] splits a list into head and tail parts. This returns [None] if the
list is empty.

Running time: O(1)

{[
  List.uncons([\"a\", \"b\", \"c\"]) == Some((\"a\", [\"b\", \"c\"]));
  List.uncons([0]) == Some((0, []));
  List.uncons([]) == None;
]}
")
let uncons: 'a. list<'a> => option<('a, list<'a>)> = x =>
  switch x {
  | list{} => None
  | list{x, ...xs} => Some((x, xs))
  }

@ocaml.doc("
[List.append] adds an item to the end of the list. This requires a full copy to
be made of the provided list, which means that the returned list will have no
relationship in memory to the original list.

Running time: O(n)

{[
  List.append(4, [1, 2, 3]) == [1, 2, 3, 4];
]}
")
let append: 'a. ('a, list<'a>) => list<'a> = (x, xs) =>
  Relude_List_Instances.SemigroupAny.append(xs, list{x})

@ocaml.doc("
[List.appendOption] adds an item to the end of the list when it is Some.
This requires a full copy to be made of the provided list,
which means that the returned list will have no relationship in
memory to the original list.

Running time: O(n)

{[
  List.appendOption(Some(4), [1, 2, 3]) == [1, 2, 3, 4];
  List.appendOption(None, [1, 2, 3]) == [1, 2, 3];
]}
")
let appendOption: 'a. (option<'a>, list<'a>) => list<'a> = (x, xs) =>
  x |> Relude_Option_Base.fold(xs, append(_, xs))

@ocaml.doc("
[List.repeat] accepts a count and a value, and it creates a new list that
contains the provided value, duplicated [count] times.

Note that a negative count will return an empty list.

Running time: O(n) (where n is the provided count)

{[
  List.repeat(3, \"hello\") == [\"hello\", \"hello\", \"hello\"];
  List.repeat(-1, 42) == [];
]}
")
let repeat: 'a. (int, 'a) => list<'a> = (i, x) => Belt.List.make(i, x)

@ocaml.doc("
Makes a list by mapping a function over a range of ints from [0] to [[n - 1]].
")
let makeWithIndex: 'a. (int, int => 'a) => list<'a> = Belt.List.makeBy

@ocaml.doc("
Maps an indexed function over the values of a list to produce a new list.
")
let mapWithIndex: 'a 'b. (('a, int) => 'b, list<'a>) => list<'b> = (f, xs) =>
  Belt.List.mapWithIndex(xs, (i, x) => f(x, i))

@ocaml.doc("
Reverses the given list
")
let reverse: 'a. list<'a> => list<'a> = Belt.List.reverse

@ocaml.doc("
Shuffles the given list to create a new list
")
let shuffle: 'a. list<'a> => list<'a> = Belt.List.shuffle

@ocaml.doc("
Indicates if the given list is empty ([length == 0]).
")
let isEmpty: 'a. list<'a> => bool = x =>
  switch x {
  | list{} => true
  | _ => false
  }

@ocaml.doc("
Indicates if the given list is non-empty ([length > 0]).
")
let isNotEmpty: 'a. list<'a> => bool = xs => !isEmpty(xs)

@ocaml.doc("
Gets the value at the given index of the list, or None if the index is out of
range.
")
let at: 'a. (int, list<'a>) => option<'a> = (i, xs) => Belt.List.get(xs, i)

@ocaml.doc("
Gets the first item of the list, or None if the list is empty.
")
let head: 'a. list<'a> => option<'a> = x =>
  switch x {
  | list{} => None
  | list{x, ..._} => Some(x)
  }

@ocaml.doc("
Gets all but the first items in the list, or [None] if the list is empty.
")
let tail: 'a. list<'a> => option<list<'a>> = x =>
  switch x {
  | list{} => None
  | list{_, ...xs} => Some(xs)
  }

@ocaml.doc("
Gets all but the first items in the list, or [[]] if the list is empty.
")
let tailOrEmpty: 'a. list<'a> => list<'a> = xs => tail(xs) |> Relude_Option_Base.getOrElse(list{})

@ocaml.doc("
Gets all but the last item in the list, or None if the list is empty
")
let rec init: 'a. list<'a> => option<list<'a>> = x =>
  switch x {
  | list{} => None
  | list{_} => Some(list{})
  | list{x, ...xs} => Some(cons(x, Relude_Option_Base.getOrElse(list{}, init(xs))))
  }

@ocaml.doc("
Gets all but the last item in the list, or [[]] if the list is empty
")
let initOrEmpty: list<'a> => list<'a> = xs =>
  switch init(xs) {
  | Some(ys) => ys
  | None => list{}
  }

@ocaml.doc("
Gets the last item of the list, or [None] if the list is empty.
")
let rec last: 'a. list<'a> => option<'a> = x =>
  switch x {
  | list{} => None
  | list{x} => Some(x)
  | list{_, ...xs} => last(xs)
  }

@ocaml.doc("
Creates a new list by taking up to [n] items from the given list.
")
let take: 'a. (int, list<'a>) => list<'a> = (i, xs) => {
  let rec go = (acc, count, rest) =>
    switch rest {
    | _ if count <= 0 => acc
    | list{} => acc
    | list{y, ...ys} => go(list{y, ...acc}, count - 1, ys)
    }
  go(list{}, i, xs) |> reverse
}

@ocaml.doc("
Creates a new list by taking exactly [n] items from the given list. If there are
not at least [n] items, the result is None.
")
let takeExactly: 'a. (int, list<'a>) => option<list<'a>> = (i, xs) => {
  let rec go = (acc, count, rest) =>
    switch rest {
    | _ if count <= 0 => Some(acc)
    | list{} => None
    | list{y, ...ys} => go(list{y, ...acc}, count - 1, ys)
    }
  if i >= 0 {
    go(list{}, i, xs) |> Relude_Option_Instances.map(reverse)
  } else {
    None
  }
}

@ocaml.doc("
Creates a new list by taking items from the list until an item is reached that
does not satisfy the given predicate.
")
let takeWhile: 'a. ('a => bool, list<'a>) => list<'a> = (f, xs) => {
  let rec go = (acc, rest) =>
    switch rest {
    | list{} => acc
    | list{y, ..._} if !f(y) => acc
    | list{y, ...ys} => go(list{y, ...acc}, ys)
    }
  go(list{}, xs) |> reverse
}

@ocaml.doc("
Creates a new list by dropping up to [n] items from the given list.
")
let rec drop: 'a. (int, list<'a>) => list<'a> = (i, xs) =>
  switch xs {
  | list{} => list{}
  | list{_, ..._} if i <= 0 => xs
  | list{_, ...ys} => drop(i - 1, ys)
  }

@ocaml.doc("
Creates a new list by dropping exactly [n] items from the given list. If there
are fewer than [n] items, None is returned.
")
let dropExactly: 'a. (int, list<'a>) => option<list<'a>> = (i, xs) => Belt.List.drop(xs, i)

@ocaml.doc("
Creates a new list by dropping items from the list until an item is reached
which does not satisfy the given predicate.
")
let rec dropWhile: 'a. ('a => bool, list<'a>) => list<'a> = (f, xs) =>
  switch xs {
  | list{y, ...ys} if f(y) => dropWhile(f, ys)
  | _ => xs
  }

@ocaml.doc("
Creates a new list containing only the items from the given list that satisfy
the given predicate.
")
let filter: 'a. ('a => bool, list<'a>) => list<'a> = (f, xs) => Belt.List.keep(xs, f)

@ocaml.doc("
Alias of filter
")
let keep: 'a. ('a => bool, list<'a>) => list<'a> = filter

@ocaml.doc("
Creates a new list containing only the items from the given list that satisfy
the given indexed predicate.
")
let filterWithIndex: 'a. (('a, int) => bool, list<'a>) => list<'a> = (f, xs) =>
  Belt.List.keepWithIndex(xs, f)

@ocaml.doc("
Alias of filterWithIndex
")
let keepWithIndex: 'a. (('a, int) => bool, list<'a>) => list<'a> = filterWithIndex

@ocaml.doc("
Creates a new list containing only the items from the given list that do not
satisfy the given predicate.
")
let filterNot: 'a. ('a => bool, list<'a>) => list<'a> = f => filter(a => !f(a))

@ocaml.doc("
Alias of filterNot
")
let reject: 'a. ('a => bool, list<'a>) => list<'a> = filterNot

@ocaml.doc("
Creates a new list containing only the items from the given list that do not
satisfy the given indexed predicate.
")
let filterNotWithIndex: 'a. (('a, int) => bool, list<'a>) => list<'a> = f =>
  filterWithIndex((a, i) => !f(a, i))

@ocaml.doc("
Alias of filterNotWithIndex
")
let rejectWithIndex: 'a. (('a, int) => bool, list<'a>) => list<'a> = filterNotWithIndex

@ocaml.doc("
Maps a option-creating function over the list, and keeps the [Some] values
without the [Some] structure.
")
let mapOption: 'a 'b. ('a => option<'b>, list<'a>) => list<'b> = (f, xs) =>
  Relude_List_Instances.foldLeft(
    (acc, curr) => Relude_Option_Base.fold(acc, v => list{v, ...acc}, f(curr)),
    list{},
    xs,
  ) |> reverse

@ocaml.doc("
Keeps all the Some values from the list, and removes the Some structure.
")
let catOptions: 'a. list<option<'a>> => list<'a> = xs => mapOption(a => a, xs)

@ocaml.doc("
Partitions the given list into two lists, one (left-side) containing values that
satisfy the given predicate and one
(right-side) containing values that do not satisfy the predicate.
")
let partition: 'a. ('a => bool, list<'a>) => (list<'a>, list<'a>) = (f, xs) =>
  Belt.List.partition(xs, f)

@ocaml.doc("
Splits a list into two lists at the given index.  None is returned if the index
is out of range.
")
let splitAt: 'a. (int, list<'a>) => option<(list<'a>, list<'a>)> = (i, xs) =>
  Belt.List.splitAt(xs, i)

@ocaml.doc("
Creates a new list by prepending the given value to each item in the list.
")
let prependToAll: 'a. ('a, list<'a>) => list<'a> = (delim, xs) => {
  let rec go = (acc, x) =>
    switch x {
    | list{} => acc
    | list{y, ...ys} => go(list{y, delim, ...acc}, ys)
    }

  go(list{}, xs) |> reverse
}

@ocaml.doc("
Creates a new list by inserting the given item in between each item in the list.
")
let intersperse: 'a. ('a, list<'a>) => list<'a> = (delim, xs) =>
  switch xs {
  | list{} => list{}
  | list{y, ...ys} => list{y, ...prependToAll(delim, ys)}
  }

@ocaml.doc("
Creates a new list by copying the given list [n] times.
")
let replicate: 'a. (int, list<'a>) => list<'a> = (i, xs) => {
  let rec go = (count, acc) =>
    count <= 1 ? acc : go(count - 1, Relude_List_Instances.concat(xs, acc))
  if i <= 0 {
    list{}
  } else {
    go(i, xs)
  }
}

@ocaml.doc("
Combines two lists pair-wise into a list of tuple-2
")
let zip: 'a 'b. (list<'a>, list<'b>) => list<('a, 'b)> = Belt.List.zip

@ocaml.doc("
Combines two lists using a function to combine pair-wise values.
")
let zipWith: 'a 'b 'c. (('a, 'b) => 'c, list<'a>, list<'b>) => list<'c> = (f, xs, ys) =>
  Belt.List.zipBy(xs, ys, f)

@ocaml.doc("
Creates a new list with each item paired with its index in the list.
")
let zipWithIndex: 'a. list<'a> => list<('a, int)> = xs => mapWithIndex((v, i) => (v, i), xs)

@ocaml.doc("
Creates two lists by splitting a list of tuple-2 on the left and right.
")
let unzip: 'a 'b. list<('a, 'b)> => (list<'a>, list<'b>) = Belt.List.unzip

@ocaml.doc("
Sorts a list with the given int-based compare function.
")
let sortWithInt: 'a. (('a, 'a) => int, list<'a>) => list<'a> = (f, xs) => Belt.List.sort(xs, f)

@ocaml.doc("
Sorts a list with the given ordering-based compare function.
")
let sortBy: 'a. (('a, 'a) => ordering, list<'a>) => list<'a> = (f, xs) =>
  sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs)

@ocaml.doc("
Sorts a list with the given ORD module.
")
let sort = (type a, ordA: module(ORD with type t = a), xs: list<a>): list<a> => {
  module OrdA = unpack(ordA)
  sortBy(OrdA.compare, xs)
}

@ocaml.doc("
Creates a new list containing only the distinct items of the given list, using
the given equality function.
")
let distinctBy: 'a. (('a, 'a) => bool, list<'a>) => list<'a> = (eq, xs) =>
  Relude_List_Instances.foldLeft(
    (ys, x) => Relude_List_Instances.containsBy(eq, x, ys) ? ys : list{x, ...ys},
    list{},
    xs,
  ) |> reverse

@ocaml.doc("
Removes the first occurrence of the given item from the list, based on the given
equality function.
")
let removeFirstBy: 'a. (('a, 'a) => bool, 'a, list<'a>) => list<'a> = (innerEq, v, xs) => {
  let go = ((found, ys), x) =>
    found ? (true, list{x, ...ys}) : innerEq(v, x) ? (true, ys) : (false, list{x, ...ys})
  Relude_List_Instances.foldLeft(go, (false, list{}), xs) |> snd |> reverse
}

@ocaml.doc("
Removes all occurrences of the given item from the list, based on the given
equality function.
")
let removeEachBy: 'a. (('a, 'a) => bool, 'a, list<'a>) => list<'a> = (innerEq, x, xs) =>
  Relude_List_Instances.foldLeft(
    (ys, y) => innerEq(x, y) ? ys : list{y, ...ys},
    list{},
    xs,
  ) |> reverse

@ocaml.doc("
Creates a new list containing only the distinct values of the list, based on the
given equality function.
")
let distinct = (type a, eqA: module(EQ with type t = a), xs) => {
  module EqA = unpack(eqA)
  distinctBy(EqA.eq, xs)
}

@ocaml.doc("
Removes the first occurrence of the given item from the list, based on the given
EQ module.
")
let removeFirst = (type a, eqA: module(EQ with type t = a), x, xs) => {
  module EqA = unpack(eqA)
  removeFirstBy(EqA.eq, x, xs)
}

@ocaml.doc("
Removes all occurrences of the given item from the list, based on the given EQ
module.
")
let removeEach = (type a, eqA: module(EQ with type t = a), x, xs) => {
  module EqA = unpack(eqA)
  removeEachBy(EqA.eq, x, xs)
}

@ocaml.doc("
Creates a new list that replaces the item at the given index with the given
value. If the index is out of range, no replacement is made.
")
let replaceAt: 'a. (int, 'a, list<'a>) => list<'a> = (targetIndex, newX, xs) =>
  xs |> mapWithIndex((x, currentIndex) =>
    if currentIndex == targetIndex {
      newX
    } else {
      x
    }
  )

@ocaml.doc("
Similar to foldLeft, but collects the results from each iteration,
rather than accumulating a single final result.
")
let scanLeft: (('b, 'a) => 'b, 'b, list<'a>) => list<'b> = (f, init, xs) =>
  Relude_List_Instances.foldLeft(((acc, result), curr) => {
    let nextAcc = f(acc, curr)
    (nextAcc, list{nextAcc, ...result})
  }, (init, list{}), xs) |> snd |> Belt.List.reverse // TODO use our own implementation

@ocaml.doc("
Similar to foldRight, but collects the results from each iteration,
rather than accumulating a single final result.
")
let scanRight: (('a, 'b) => 'b, 'b, list<'a>) => list<'b> = (f, init, xs) =>
  Relude_List_Instances.foldRight((curr, (acc, result)) => {
    let nextAcc = f(curr, acc)
    (nextAcc, list{nextAcc, ...result})
  }, (init, list{}), xs) |> snd

@ocaml.doc("
Creates a new list that inserts the given value at the given index.
If the index is out of range, no insertion is made.
")
let insertAt: 'a. (int, 'a, list<'a>) => list<'a> = (targetIndex, newX, xs) =>
  switch splitAt(targetIndex, xs) {
  | Some((before, after)) => Relude_List_Instances.concat(before, list{newX, ...after})
  | None => xs
  }

@ocaml.doc("
Creates a new list that modifies the value at the given index with the given
function. If the index is out of range, no change is made.
")
let updateAt: 'a. (int, 'a => 'a, list<'a>) => list<'a> = (targetIndex, f, xs) =>
  xs |> mapWithIndex((x, index) => index == targetIndex ? f(x) : x)

@ocaml.doc("
Creates a new list with the elements at the two given indexes swapped.
If either index is out of range, no change is made.
")
let swapAt: 'a. (int, int, list<'a>) => list<'a> = (i, j, xs) =>
  switch (at(i, xs), at(j, xs)) {
  | (Some(a), Some(b)) => xs |> mapWithIndex((x, k) => i == k ? b : j == k ? a : x)
  | _ => xs
  }

@ocaml.doc("
Creates a new list without the element at the given index. If the index is out
of range, no change is made.
")
let removeAt: 'a. (int, list<'a>) => list<'a> = (targetIndex, xs) =>
  xs |> filterWithIndex((_, i) => i != targetIndex)

@ocaml.doc("
Creates a list of elements split into groups the length of size. If the list
can't be split evenly, the final chunk will be the remaining elements.
")
let rec chunk: 'a. (int, list<'a>) => list<list<'a>> = (size, xs) =>
  xs |> Relude_List_Instances.length <= size
    ? list{xs}
    : list{xs |> take(size), ...xs |> drop(size) |> chunk(size)}
