let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <svg
      className="drag-handle-icon"
      viewBox="0 0 32 32"
      xmlns="http://www.w3.org/2000/svg">
      <title> ("Drag Handle" |> ReasonReact.string) </title>
      <path d="M2 6h28v4h-28v-4z" />
      <path d="M2 14h28v4h-28v-4z" />
      <path d="M2 22h28v4h-28v-4z" />
    </svg>,
};
