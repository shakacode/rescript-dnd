[@react.component]
let make = (~path, ~active, ~children) => {
  <button
    type_="button"
    className=Cn.("link" + "active"->on(active))
    onClick={_ => RescriptReactRouter.push("#" ++ path)}>
    children
  </button>;
};
