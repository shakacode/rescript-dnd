open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make =
    (
      ~className="",
      ~style: option(ReactDom.Style.t)=?,
      ~onClick: option(ReactEvent.Mouse.t => unit)=?,
      ~onKeyUp: option(ReactEvent.Keyboard.t => unit)=?,
      ~onKeyDown: option(ReactEvent.Keyboard.t => unit)=?,
      ~onMouseDown: option(ReactEvent.Mouse.t => unit)=?,
      ~onTouchStart: option(ReactEvent.Touch.t => unit)=?,
      children,
    ) => {
  ...component,
  render: _ =>
    <button
      type_="button"
      className={Cn.make(["control", className])}
      ?style
      ?onClick
      ?onKeyUp
      ?onKeyDown
      ?onMouseDown
      ?onTouchStart>
      ...children
    </button>,
};
