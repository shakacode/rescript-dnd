open Dnd__React;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = children => {
  ...component,
  render: _ => <main> <Fragment> ...children </Fragment> </main>,
};
