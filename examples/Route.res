type t =
  | VerticalList
  | HorizontalList
  | VerticalScrollableContainer
  | HorizontalScrollableContainer
  | CardBoard
  | NestedVerticalLists

let fromUrl = url => {
  open RescriptReactRouter
  switch url.hash {
  | "vertical-list" => VerticalList
  | "horizontal-list" => HorizontalList
  | "vertical-scrollable-container" => VerticalScrollableContainer
  | "horizontal-scrollable-container" => HorizontalScrollableContainer
  | "card-board" => CardBoard
  | "nested-vertical-lists" => NestedVerticalLists
  | _ => VerticalList
  }
}
