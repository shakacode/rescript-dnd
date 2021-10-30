@react.component
let make = (~route, ~mobileNavShown, ~hideMobileNav) =>
  <nav className={mobileNavShown ? "shown-mobile" : ""}>
    <header>
      <h1 onClick={_ => RescriptReactRouter.push("/")}> {"rescript-dnd"->React.string} </h1>
      <a href="https://github.com/shakacode/rescript-dnd" className="github-link">
        <GithubIcon />
      </a>
      <Control className="close-button" onClick={_ => hideMobileNav()}> <CloseIcon /> </Control>
    </header>
    <Link path="vertical-list" active={route == Route.VerticalList}>
      {"Vertical list"->React.string}
    </Link>
    <Link path="horizontal-list" active={route == Route.HorizontalList}>
      {"Horizontal list"->React.string}
    </Link>
    <Link path="vertical-scrollable-container" active={route == Route.VerticalScrollableContainer}>
      {"Vertical scrollable container"->React.string}
    </Link>
    <Link
      path="horizontal-scrollable-container" active={route == Route.HorizontalScrollableContainer}>
      {"Horizontal scrollable container"->React.string}
    </Link>
    <Link path="card-board" active={route == Route.CardBoard}> {"Card board"->React.string} </Link>
    <Link path="nested-vertical-lists" active={route == Route.NestedVerticalLists}>
      {"Nested vertical list"->React.string}
    </Link>
  </nav>
