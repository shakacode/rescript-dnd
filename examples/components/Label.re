let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~htmlFor, children) => {
  ...component,
  render: _ =>
    ReasonReact.createDomElement(
      "label",
      ~props={"htmlFor": htmlFor, "className": "label"},
      children,
    ),
};
