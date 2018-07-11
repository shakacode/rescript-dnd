let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~id, ~value, ~onChange, _) => {
  ...component,
  render: _ =>
    <input
      id
      className="input"
      value
      onChange=(
        event =>
          ReactDOMRe.domElementToObj(event |> ReactEventRe.Form.target)##value
          |> onChange
      )
    />,
};
