type state = {mobileNavShown: bool}

type action =
  | ShowMobileNav
  | HideMobileNav

let reducer = (_state, action) =>
  switch action {
  | ShowMobileNav => {mobileNavShown: true}
  | HideMobileNav => {mobileNavShown: false}
  }

@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()
  let route = React.useMemo1(() => url->Route.fromUrl, [url])

  let (state, dispatch) = reducer->React.useReducer({mobileNavShown: false})

  React.useEffect1(() => {
    HideMobileNav->dispatch
    None
  }, [route])

  let showMobileNav = React.useCallback(() => ShowMobileNav->dispatch)
  let hideMobileNav = React.useCallback(() => HideMobileNav->dispatch)

  <Container>
    <Nav route mobileNavShown=state.mobileNavShown hideMobileNav />
    <Main>
      {switch route {
      | VerticalList =>
        <Example.Dynamic
          key=url.hash title="Vertical list" layout=Vertical initialAmount=7 showMobileNav>
          {n => <SimpleList key={n->Int.toString} axis=Y n />}
        </Example.Dynamic>
      | HorizontalList =>
        <Example.Dynamic
          key=url.hash title="Horizontal list" layout=Horizontal initialAmount=7 showMobileNav>
          {n => <SimpleList key={n->Int.toString} axis=X n />}
        </Example.Dynamic>
      | VerticalScrollableContainer =>
        <Example.Dynamic
          key=url.hash
          title="Vertical scrollable container"
          layout=Vertical
          scrollable=true
          initialAmount=50
          showMobileNav>
          {n => <SimpleList key={n->Int.toString} axis=Y n />}
        </Example.Dynamic>
      | HorizontalScrollableContainer =>
        <Example.Dynamic
          key=url.hash
          title="Horizontal scrollable container"
          layout=Horizontal
          scrollable=true
          initialAmount=50
          showMobileNav>
          {n => <SimpleList key={n->Int.toString} axis=X n />}
        </Example.Dynamic>
      | CardBoard =>
        <Example.Static key=url.hash title="Card board" layout=CardBoard showMobileNav>
          <CardBoard />
        </Example.Static>
      | NestedVerticalLists =>
        <Example.Static key=url.hash title="Nested vertical list" layout=Vertical showMobileNav>
          <NestedVerticalLists />
        </Example.Static>
      }}
    </Main>
  </Container>
}
