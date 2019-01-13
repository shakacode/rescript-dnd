open Dnd__React;

let component = React.statelessComponent(__MODULE__);
let make = (~id, ~value, ~onChange, _) => {
  ...component,
  render: _ =>
    <input
      id
      className="input"
      value
      onChange={event => event->ReactEvent.Form.target##value->onChange}
    />,
};
