# re-dnd

[![npm version](https://img.shields.io/npm/v/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)
[![build status](https://img.shields.io/travis/alexfedoseev/re-dnd/master.svg?style=flat-square)](https://travis-ci.org/alexfedoseev/re-dnd)
[![license](https://img.shields.io/npm/l/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)

Reasonable drag-n-drop for [`reason-react`](https://reasonml.github.io/reason-react/).

<pre align="center">
🚧 === The library is in active development and not production ready yet === 🚧
🚧 ===               Breaking changes are highly expected                === 🚧
</pre>


## Features
* Vertical lists
* Multiple drop targets
* Mouse & Touch interactions
* Conditional drag & drop
* Custom drag handles
* Auto-scroll when dragging at window's edge

### TODO
- [ ] Perf optimizations
- [ ] Scrollable containers
- [ ] Horizontal lists
- [ ] Keyboard interactions
- [ ] Ignore form elements (opt-out)
- [ ] Drop-zones
- [ ] Multi-select

## Installation

```shell
# yarn
yarn add re-dnd
# or npm
npm install --save re-dnd
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "re-dnd"
]
```

## Examples

Live: [`re-dnd.now.sh`](https://re-dnd.now.sh)  
Src: [`./examples`](./examples)

Quickie:

```reason
module Cfg = {
  module Draggable = {
    type t =
      | Todo(int);

    let eq = (d1, d2) =>
      switch (d1, d2) {
      | (Todo(id1), Todo(id2)) => id1 === id2
      };
  };

  module Droppable = {
    type t =
      | TodoList;

    let eq = (_, _) => true;
  };
};

module Screen = Dnd.Make(Cfg);

type state = {
  todos: array(Todo.t),
};

type action =
  | Reorder(Dnd.result(Cfg.Draggable.t, Cfg.Droppable.t));

let component = ReasonReact.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {todos: /* initial data */},
  reducer: (action, state) =>
    switch (action) {
    | Reorder(result) =>
      switch (result) {
      | SameTarget(Todo(id), TodoList, todos) =>
        ReasonReact.Update({...state, todos: /* update logic */})
      | NewTarget(_, _, _)
      | NoChanges => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) =>
    <Screen.Context onDrop=(result => Reorder(result) |> send)>
      ...(
           dnd =>
             <Screen.Droppable
               id=TodoList
               context=dnd.context
               className=(
                 (~draggingOver) =>
                   Cn.make(["todos", "active" |> Cn.ifTrue(draggingOver)])
               )>
               (
                 state.todos
                 |. Array.map(todo =>
                      <Screen.Draggable
                        id=(Todo(todo.id))
                        key=(todo.id |. string_of_int)
                        droppableId=TodoList
                        context=dnd.context
                        className=(
                          (~dragging) =>
                            Cn.make([
                              "todo",
                              "dragging" |> Cn.ifTrue(dragging),
                            ])
                        )>
                        ...(Children(todo.todo |> ReasonReact.string))
                      </Screen.Draggable>
                    )
                 |. ReasonReact.array
               )
             </Screen.Droppable>
         )
    </Screen.Context>,
};
```

## Thanks
* To [`react-beautiful-dnd`](https://github.com/atlassian/react-beautiful-dnd) for inspiration

## License

MIT.
