module type SelectableItem = {
  type t
  let cmp: (t, t) => int
}

module type DndEntry = {
  type t
  let eq: (t, t) => bool
  let cmp: (t, t) => int
}
