open Dnd__React;

type state = {amount: int};

type action =
  | UpdateAmount(string);

let component = React.reducerComponent(__MODULE__);
let make = (~startWith, children) => {
  ...component,
  initialState: () => {amount: startWith},
  reducer: (action, _state) =>
    switch (action) {
    | UpdateAmount("") => React.Update({amount: 0})
    | UpdateAmount(value) =>
      switch (value->int_of_string) {
      | value when value >= 1_000 => React.NoUpdate
      | value => React.Update({amount: value})
      | exception _ => React.NoUpdate
      }
    },
  render: ({state, send}) =>
    children(~amount=state.amount, ~updateAmount=value =>
      UpdateAmount(value)->send
    ),
};
