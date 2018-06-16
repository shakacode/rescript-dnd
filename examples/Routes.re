type route =
  | VerticalList
  | VerticalListMediumDataset
  | NestedVerticalLists;

let getRoute = url =>
  ReasonReact.Router.(
    switch (url.hash) {
    | "vertical-list" => VerticalList
    | "vertical-list-medium-dataset" => VerticalListMediumDataset
    | "nested-vertical-lists" => NestedVerticalLists
    | _ => VerticalList
    }
  );
