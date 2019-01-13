open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = (~path, ~active, children) => {
  ...component,
  render: _ =>
    <button
      className={Cn.make(["link", "active"->Cn.ifTrue(active)])}
      onClick={_ => React.Router.push("#" ++ path)}>
      ...children
    </button>,
};
