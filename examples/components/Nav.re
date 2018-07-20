let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~route, ~mobileNavShown, ~hideMobileNav, _) => {
  ...component,
  render: _ =>
    <nav className=(Cn.make(["shown-mobile" |> Cn.ifTrue(mobileNavShown)]))>
      <header>
        <h1 onClick=(_ => ReasonReact.Router.push("/"))>
          ("re-dnd" |> ReasonReact.string)
        </h1>
        <a
          href="https://github.com/alexfedoseev/re-dnd"
          className="github-link">
          <GithubIcon />
        </a>
        <Control className="close-button" onClick=hideMobileNav>
          <CloseIcon />
        </Control>
      </header>
      <Link path="vertical-list" active=(route === Routes.VerticalList)>
        ("Vertical list" |> ReasonReact.string)
      </Link>
      <Link path="horizontal-list" active=(route === Routes.HorizontalList)>
        ("Horizontal list" |> ReasonReact.string)
      </Link>
      <Link
        path="vertical-scrollable-container"
        active=(route === Routes.VerticalScrollableContainer)>
        ("Vertical scrollable container" |> ReasonReact.string)
      </Link>
      <Link
        path="horizontal-scrollable-container"
        active=(route === Routes.HorizontalScrollableContainer)>
        ("Horizontal scrollable container" |> ReasonReact.string)
      </Link>
      <Link path="card-board" active=(route === Routes.CardBoard)>
        ("Card board" |> ReasonReact.string)
      </Link>
      <Link
        path="nested-vertical-lists"
        active=(route === Routes.NestedVerticalLists)>
        ("Nested vertical list" |> ReasonReact.string)
      </Link>
    </nav>,
};
