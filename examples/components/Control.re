open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~className="", ~onClick: ReactEventRe.Mouse.t => unit, children) => {
  ...component,
  render: _ =>
    <button
      type_="button" className=(Cn.make(["control", className])) onClick>
      <Fragment> ...children </Fragment>
    </button>,
};
