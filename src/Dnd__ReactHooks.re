module Previous = {
  let hook = (v: 'a): option('a) => {
    let x = React.useRef(None);
    React.useEffect(() => {
      x.current = v->Some;
      None;
    });
    x.current;
  };
};

module Reducer = {
  module Dispatch = {
    type t('action) = 'action => unit;
  };

  module Public = {
    type t('state, 'action) = {
      state: 'state,
      dispatch: Dispatch.t('action),
    };
  };

  module Private = {
    type t('state, 'action) = {
      state: 'state,
      sideEffects: ref(array(Public.t('state, 'action) => unit)),
    };
  };

  type update('state, 'action) =
    | NoUpdate
    | Update('state)
    | UpdateWithSideEffects('state, Public.t('state, 'action) => unit)
    | SideEffects(Public.t('state, 'action) => unit);

  let hook = (initialState, reducer) => {
    let ({Private.state, sideEffects}, dispatch) =
      React.useReducer(
        ({Private.state, sideEffects} as private, action) =>
          switch (reducer(state, action)) {
          | NoUpdate => private
          | Update(state) => {...private, state}
          | UpdateWithSideEffects(state, sideEffect) => {
              state,
              sideEffects: Array.concat(sideEffects^, [|sideEffect|])->ref,
            }
          | SideEffects(sideEffect) => {
              ...private,
              sideEffects: Array.concat(sideEffects^, [|sideEffect|])->ref,
            }
          },
        {state: initialState, sideEffects: [||]->ref},
      );
    React.useEffect1(
      () => {
        if (Array.length(sideEffects^) > 0) {
          Array.forEach(sideEffects^, fn => fn({state, dispatch}));
          sideEffects := [||];
        };
        None;
      },
      [|sideEffects^|],
    );
    (state, dispatch);
  };
};

let usePrevious = Previous.hook;
let useReducer = Reducer.hook;
