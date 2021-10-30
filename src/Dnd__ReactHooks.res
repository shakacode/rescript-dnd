module Previous = {
  let hook = (v: 'a): option<'a> => {
    let x = React.useRef(None)
    React.useEffect(() => {
      x.current = v->Some
      None
    })
    x.current
  }
}

module Reducer = {
  module Dispatch = {
    type t<'action> = 'action => unit
  }

  module Public = {
    type t<'state, 'action> = {
      state: 'state,
      dispatch: Dispatch.t<'action>,
    }
  }

  module Private = {
    type t<'state, 'action> = {
      state: 'state,
      sideEffects: ref<array<Public.t<'state, 'action> => unit>>,
    }
  }

  type update<'state, 'action> =
    | NoUpdate
    | Update('state)
    | UpdateWithSideEffects('state, Public.t<'state, 'action> => unit)
    | SideEffects(Public.t<'state, 'action> => unit)

  let hook = (initialState, reducer) => {
    let ({Private.state: state, sideEffects}, dispatch) = React.useReducer(
      ({Private.state: state, sideEffects} as private_, action) =>
        switch reducer(state, action) {
        | NoUpdate => private_
        | Update(state) => {...private_, state: state}
        | UpdateWithSideEffects(state, sideEffect) => {
            state: state,
            sideEffects: Array.concat(sideEffects.contents, [sideEffect])->ref,
          }
        | SideEffects(sideEffect) => {
            ...private_,
            sideEffects: Array.concat(sideEffects.contents, [sideEffect])->ref,
          }
        },
      {state: initialState, sideEffects: []->ref},
    )
    React.useEffect1(() => {
      if Array.length(sideEffects.contents) > 0 {
        Array.forEach(sideEffects.contents, fn => fn({state: state, dispatch: dispatch}))
        sideEffects := []
      }
      None
    }, [sideEffects.contents])
    (state, dispatch)
  }
}

let usePrevious = Previous.hook
let useReducer = Reducer.hook
