type state = {amount: int};

type action =
  | UpdateAmount(string);

let component = ReasonReact.reducerComponent(__MODULE__);
let make = (~startWith, children) => {
  ...component,
  initialState: () => {amount: startWith},
  reducer: (action, _state) =>
    switch (action) {
    | UpdateAmount("") => ReasonReact.Update({amount: 0})
    | UpdateAmount(value) =>
      switch (value |> int_of_string) {
      | value when value >= 1_000 => ReasonReact.NoUpdate
      | value => ReasonReact.Update({amount: value})
      | exception _ => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) =>
    children(~amount=state.amount, ~updateAmount=value =>
      UpdateAmount(value) |> send
    ),
};
