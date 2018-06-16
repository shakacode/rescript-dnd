open Dnd__React;

type layout =
  | Vertical
  | Horizontal;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~layout, children) => {
  ...component,
  render: _ =>
    <div
      className=(
        Cn.make([
          "example",
          switch (layout) {
          | Vertical => "vertical"
          | Horizontal => "horizontal"
          },
        ])
      )>
      <Fragment> ...children </Fragment>
    </div>,
};
