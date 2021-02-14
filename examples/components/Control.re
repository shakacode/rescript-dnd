[@react.component]
let make =
    (
      ~className="",
      ~style: option(ReactDOM.Style.t)=?,
      ~onClick: option(ReactEvent.Mouse.t => unit)=?,
      ~onMouseDown: option(ReactEvent.Mouse.t => unit)=?,
      ~onTouchStart: option(ReactEvent.Touch.t => unit)=?,
      ~children,
    ) => {
  <button
    type_="button"
    className=Cn.("control" + className)
    ?style
    ?onClick
    ?onMouseDown
    ?onTouchStart>
    children
  </button>;
};
