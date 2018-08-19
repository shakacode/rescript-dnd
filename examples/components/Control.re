open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make =
    (
      ~className="",
      ~style: option(ReactDOMRe.Style.t)=?,
      ~onClick: option(ReactEventRe.Mouse.t => unit)=?,
      ~onKeyUp: option(ReactEventRe.Keyboard.t => unit)=?,
      ~onKeyDown: option(ReactEventRe.Keyboard.t => unit)=?,
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
      ?onKeyUp
      ?onKeyDown
      ?onMouseDown
      ?onTouchStart>
      <Fragment> ...children </Fragment>
    </button>,
};
