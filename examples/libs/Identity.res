module Make = () => {
  module Id: {
    type t
  } = {
    type t = int
  }

  include Id

  external make: int => t = "%identity"
  external array: array<int> => array<t> = "%identity"
  external toInt: t => int = "%identity"

  let toString = x => x->toInt->Int.toString

  let eq = (x1, x2) => x1->toInt == x2->toInt
  let cmp = (x1, x2) => Pervasives.compare(x1->toInt, x2->toInt)

  module Comparable = Belt.Id.MakeComparable({
    type t = Id.t
    let cmp = cmp
  })

  module Map = {
    type t<'t> = Map.t<Id.t, 't, Comparable.identity>
    let make = () => Map.make(~id=module(Comparable))
  }
}
