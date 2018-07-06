type state = {
  route: Routes.route,
  mobileNavShown: bool,
};

type action =
  | UpdateRoute(Routes.route)
  | ShowMobileNav
  | HideMobileNav;

let component = ReasonReact.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {
    route: ReasonReact.Router.dangerouslyGetInitialUrl() |> Routes.getRoute,
    mobileNavShown: false,
  },
  didMount: ({send, onUnmount}) => {
    let watcher =
      ReasonReact.Router.watchUrl(url =>
        UpdateRoute(url |> Routes.getRoute) |> send
      );
    onUnmount(() => watcher |> ReasonReact.Router.unwatchUrl);
  },
  reducer: (action, state) =>
    switch (action) {
    | UpdateRoute(route) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, route},
        (
          ({state, send}) =>
            if (state.mobileNavShown) {
              HideMobileNav |> send;
            }
        ),
      )
    | ShowMobileNav => ReasonReact.Update({...state, mobileNavShown: true})
    | HideMobileNav => ReasonReact.Update({...state, mobileNavShown: false})
    },
  render: ({state, send}) => {
    let showMobileNav = _ => ShowMobileNav |> send;
    let hideMobileNav = _ => HideMobileNav |> send;

    <Layout>
      <Nav
        route=state.route
        mobileNavShown=state.mobileNavShown
        hideMobileNav
      />
      <Main>
        (
          switch (state.route) {
          | VerticalList =>
            <Example layout=Vertical showMobileNav>
              <VerticalListExample n=7 key="7" />
            </Example>
          | VerticalListMediumDataset =>
            <Example layout=Vertical showMobileNav>
              <VerticalListExample n=200 key="200" />
            </Example>
          | NestedVerticalLists =>
            <Example layout=Vertical showMobileNav>
              <NestedVerticalListsExample />
            </Example>
          | ScrollableContainer =>
            <Example layout=Vertical showMobileNav>
              <ScrollableContainerExample />
            </Example>
          }
        )
      </Main>
    </Layout>;
  },
};
