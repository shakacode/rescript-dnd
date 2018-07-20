open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = children => {
  ...component,
  render: _ =>
    <div className="container"> <Fragment> ...children </Fragment> </div>,
};
