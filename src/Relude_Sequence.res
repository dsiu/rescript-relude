module List: Relude_Interface.SEQUENCE with type t<'a> = list<'a> = {
  type t<'a> = list<'a>
  let emptyLazy = () => list{}
  let length = Relude_List_Instances.length
  let isEmpty = Relude_List_Base.isEmpty
  let isNotEmpty = Relude_List_Base.isNotEmpty
  let head = Relude_List_Base.head
  let tail = Relude_List_Base.tail
  let tailOrEmpty = Relude_List_Base.tailOrEmpty
  let uncons = Relude_List_Base.uncons
  let prepend = Relude_List_Base.prepend
  let append = Relude_List_Base.append
  let concat = Relude_List_Instances.concat
  let reverse = Relude_List_Base.reverse
  let zip = Relude_List_Base.zip
  let zipWith = Relude_List_Base.zipWith
  let fromArray = Relude_List_Instances.fromArray
  let fromList = a => a
  let toArray = Relude_List_Instances.toArray
  let toList = a => a
  let eqBy = Relude_List_Instances.eqBy
  let showBy = Relude_List_Instances.showBy
  let mkString = Relude_List_Instances.intercalate(module(BsBastet.String.Monoid))
  module Functor = Relude_List_Instances.Functor
  module Apply = Relude_List_Instances.Apply
  module Applicative = Relude_List_Instances.Applicative
  module Monad = Relude_List_Instances.Monad
  module Foldable = Relude_List_Instances.Foldable
  module Traversable = Relude_List_Instances.Traversable
  module Eq = Relude_List_Instances.Eq
  module Show = Relude_List_Instances.Show
}

module Array: Relude_Interface.SEQUENCE with type t<'a> = array<'a> = {
  type t<'a> = array<'a>
  let emptyLazy = () => []
  let length = Relude_Array_Base.length
  let isEmpty = Relude_Array_Base.isEmpty
  let isNotEmpty = Relude_Array_Base.isNotEmpty
  let head = Relude_Array_Base.head
  let tail = Relude_Array_Base.tail
  let tailOrEmpty = Relude_Array_Base.tailOrEmpty
  let uncons = Relude_Array_Base.uncons
  let prepend = Relude_Array_Base.prepend
  let append = Relude_Array_Base.append
  let concat = Relude_Array_Instances.concat
  let reverse = Relude_Array_Base.reverse
  let zip = Relude_Array_Base.zip
  let zipWith = Relude_Array_Base.zipWith
  let fromArray = a => a
  let fromList = Relude_Array_Instances.fromList
  let toArray = a => a
  let toList = Relude_Array_Instances.toList
  let eqBy = Relude_Array_Instances.eqBy
  let showBy = Relude_Array_Instances.showBy
  let mkString = Relude_Array_Instances.intercalate(module(BsBastet.String.Monoid))
  module Functor = Relude_Array_Instances.Functor
  module Apply = Relude_Array_Instances.Apply
  module Applicative = Relude_Array_Instances.Applicative
  module Monad = Relude_Array_Instances.Monad
  module Foldable = Relude_Array_Instances.Foldable
  module Traversable = Relude_Array_Instances.Traversable
  module Eq = Relude_Array_Instances.Eq
  module Show = Relude_Array_Instances.Show
}