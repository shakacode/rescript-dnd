let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <div className="scrollable-container"> <VerticalListExample n=50 /> </div>,
};
