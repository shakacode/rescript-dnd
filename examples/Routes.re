open Dnd__React;

type route =
  | VerticalList
  | HorizontalList
  | VerticalScrollableContainer
  | HorizontalScrollableContainer
  | CardBoard
  | NestedVerticalLists;

let getRoute = url =>
  React.Router.(
    switch (url.hash) {
    | "vertical-list" => VerticalList
    | "horizontal-list" => HorizontalList
    | "vertical-scrollable-container" => VerticalScrollableContainer
    | "horizontal-scrollable-container" => HorizontalScrollableContainer
    | "card-board" => CardBoard
    | "nested-vertical-lists" => NestedVerticalLists
    | _ => VerticalList
    }
  );
