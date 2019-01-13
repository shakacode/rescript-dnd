open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = _ => {
  ...component,
  render: _ =>
    <div className="example-wrapper">
      <div className="example-header">
        <div className="example-header-title">
          "Nested vertical list"->React.string
        </div>
      </div>
      <NestedVerticalListsContainer />
    </div>,
};
