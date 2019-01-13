open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = children => {
  ...component,
  render: _ => <div className="container"> ...children </div>,
};
