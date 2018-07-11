type route =
  | VerticalList
  | NestedVerticalLists
  | ScrollableContainer;

let getRoute = url =>
  ReasonReact.Router.(
    switch (url.hash) {
    | "vertical-list" => VerticalList
    | "nested-vertical-lists" => NestedVerticalLists
    | "scrollable-container" => ScrollableContainer
    | _ => VerticalList
    }
  );
