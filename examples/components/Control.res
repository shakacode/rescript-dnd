open Cx

@react.component
let make = (
  ~className="",
  ~style: option<ReactDOM.Style.t>=?,
  ~onClick: option<ReactEvent.Mouse.t => unit>=?,
  ~onMouseDown: option<ReactEvent.Mouse.t => unit>=?,
  ~onTouchStart: option<ReactEvent.Touch.t => unit>=?,
  ~children,
) =>
  <button
    type_="button"
    className={cx(["control", className])}
    ?style
    ?onClick
    ?onMouseDown
    ?onTouchStart>
    children
  </button>
