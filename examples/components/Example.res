open Cx

type layout =
  | Vertical
  | Horizontal
  | CardBoard

module Container = {
  @react.component
  let make = (~layout: layout, ~showMobileNav: unit => unit, ~children: React.element) =>
    <div
      className={cx([
        "example",
        switch layout {
        | Vertical => "vertical"
        | Horizontal => "horizontal"
        | CardBoard => "card-board"
        },
      ])}>
      <div className="example-navbar-mobile">
        <Control onClick={_ => showMobileNav()}> <MenuIcon /> </Control>
        <h1> {"rescript-dnd"->React.string} </h1>
        <a href="https://github.com/shakacode/rescript-dnd"> <GithubIcon /> </a>
      </div>
      children
    </div>
}

module Static = {
  @react.component
  let make = (
    ~title: string,
    ~layout: layout,
    ~showMobileNav: unit => unit,
    ~children: React.element,
  ) =>
    <Container layout showMobileNav>
      <div className="example-wrapper">
        <div className="example-header">
          <div className="example-header-title"> {title->React.string} </div>
        </div>
        children
      </div>
    </Container>
}

module Dynamic = {
  type amount = int

  type action = UpdateAmount(string)

  let reducer = (state, action) =>
    switch action {
    | UpdateAmount("") => 0
    | UpdateAmount(value) =>
      switch value->Int.fromString {
      | Some(value) if value >= 1_000 => state
      | Some(value) => value
      | None => state
      | exception _ => state
      }
    }

  @react.component
  let make = (
    ~title: string,
    ~layout: layout,
    ~scrollable: bool=false,
    ~showMobileNav: unit => unit,
    ~initialAmount: int,
    ~children: int => React.element,
  ) => {
    let (amount, dispatch) = reducer->React.useReducer(initialAmount)

    <Container layout showMobileNav>
      <div className="example-wrapper">
        <div className="example-header">
          <div className="example-header-title"> {title->React.string} </div>
          <div className="example-header-toolbar">
            <Label htmlFor="input-amount"> {"# of todos"->React.string} </Label>
            <Input
              id="input-amount"
              value={switch amount {
              | 0 => ""
              | _ as x => x->Int.toString
              }}
              onChange={x => UpdateAmount(x)->dispatch}
            />
          </div>
        </div>
        {if scrollable {
          <div className="scrollable-container"> {amount->children} </div>
        } else {
          amount->children
        }}
      </div>
    </Container>
  }
}
