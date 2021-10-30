open Cx

@react.component
let make = (~path, ~active, ~children) =>
  <button
    type_="button"
    className={cx(["link", active ? "active" : ""])}
    onClick={_ => RescriptReactRouter.push("#" ++ path)}>
    children
  </button>
