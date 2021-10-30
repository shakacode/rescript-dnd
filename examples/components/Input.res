@react.component
let make = (~id, ~value, ~onChange) =>
  <input
    id
    className="input"
    value
    onChange={event => (event->ReactEvent.Form.target)["value"]->onChange}
  />
