open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = (~htmlFor, children) => {
  ...component,
  render: _ => <label htmlFor className="label"> ...children </label>,
};
