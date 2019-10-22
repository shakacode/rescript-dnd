[@react.component]
let make = (~path, ~active, ~children) => {
  <button
    type_="button"
    className={Cn.make(["link", "active"->Cn.ifTrue(active)])}
    onClick={_ => ReasonReactRouter.push("#" ++ path)}>
    children
  </button>;
};
