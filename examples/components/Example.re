open Dnd__React;

type layout =
  | Vertical
  | Horizontal;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~layout, ~showMobileNav, children) => {
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
      <div className="example-navbar-mobile">
        <Control onClick=showMobileNav> <MenuIcon /> </Control>
        <h1> ("re-dnd" |> ReasonReact.string) </h1>
        <a href="https://github.com/alexfedoseev/re-dnd"> <GithubIcon /> </a>
      </div>
      <Fragment> ...children </Fragment>
    </div>,
};
