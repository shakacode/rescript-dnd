let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <div className="example-wrapper">
      <div className="example-header">
        <div className="example-header-title">
          ("Nested vertical list" |> ReasonReact.string)
        </div>
      </div>
      <NestedVerticalListsContainer />
    </div>,
};
