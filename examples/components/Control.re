open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make =
    (
      ~className="",
      ~style: option(ReactDOMRe.Style.t)=?,
      ~onClick: option(ReactEventRe.Mouse.t => unit)=?,
      ~onMouseDown: option(ReactEventRe.Mouse.t => unit)=?,
      ~onTouchStart: option(ReactEventRe.Touch.t => unit)=?,
      children,
    ) => {
  ...component,
  render: _ =>
    <button
      type_="button"
      className=(Cn.make(["control", className]))
      ?style
      ?onClick
      ?onMouseDown
      ?onTouchStart>
      <Fragment> ...children </Fragment>
    </button>,
};
