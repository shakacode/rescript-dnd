open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = _ => {
  ...component,
  render: _ =>
    <svg
      className="menu-icon"
      viewBox="0 0 32 32"
      xmlns="http://www.w3.org/2000/svg">
      <title> "Menu"->React.string </title>
      <path d="M2 6h28v6h-28zM2 14h28v6h-28zM2 22h28v6h-28z" />
    </svg>,
};
