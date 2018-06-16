open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~path, ~active, children) => {
  ...component,
  render: _ =>
    <button
      className=(Cn.make(["link", "active" |> Cn.ifTrue(active)]))
      onClick=(_ => ReasonReact.Router.push("#" ++ path))>
      <Fragment> ...children </Fragment>
    </button>,
};
