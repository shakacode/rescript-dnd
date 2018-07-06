type route =
  | VerticalList
  | VerticalListMediumDataset
  | NestedVerticalLists
  | ScrollableContainer;

let getRoute = url =>
  ReasonReact.Router.(
    switch (url.hash) {
    | "vertical-list" => VerticalList
    | "vertical-list-medium-dataset" => VerticalListMediumDataset
    | "nested-vertical-lists" => NestedVerticalLists
    | "scrollable-container" => ScrollableContainer
    | _ => VerticalList
    }
  );
