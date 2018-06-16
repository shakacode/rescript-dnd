type state = {route: Routes.route};

type action =
  | UpdateRoute(Routes.route);

let component = ReasonReact.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {
    route: ReasonReact.Router.dangerouslyGetInitialUrl() |> Routes.getRoute,
  },
  didMount: ({send, onUnmount}) => {
    let watcher =
      ReasonReact.Router.watchUrl(url =>
        UpdateRoute(url |> Routes.getRoute) |> send
      );
    onUnmount(() => watcher |> ReasonReact.Router.unwatchUrl);
  },
  reducer: (action, _) =>
    switch (action) {
    | UpdateRoute(route) => ReasonReact.Update({route: route})
    },
  render: ({state}) =>
    <Layout>
      <Nav route=state.route />
      <Main>
        (
          switch (state.route) {
          | VerticalList =>
            <Example layout=Vertical>
              <VerticalListExample n=7 key="7" />
            </Example>
          | VerticalListMediumDataset =>
            <Example layout=Vertical>
              <VerticalListExample n=200 key="200" />
            </Example>
          | NestedVerticalLists =>
            <Example layout=Vertical> <NestedVerticalListsExample /> </Example>
          }
        )
      </Main>
    </Layout>,
};
