module React = ReasonReact;
module ReactDom = ReactDOMRe;

module Fragment = {
  [@bs.module "react"] external reactClass: React.reactClass = "Fragment";
  let make = children =>
    React.wrapJsForReason(~reactClass, ~props=Js.Obj.empty(), children);
};
