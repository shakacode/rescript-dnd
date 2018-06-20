# re-dnd

[![npm version](https://img.shields.io/npm/v/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)
[![build status](https://img.shields.io/travis/alexfedoseev/re-dnd/master.svg?style=flat-square)](https://travis-ci.org/alexfedoseev/re-dnd)
[![license](https://img.shields.io/npm/l/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)

Reasonable drag-n-drop for [`reason-react`](https://reasonml.github.io/reason-react/).

` 🚧 === The library is in active development === 🚧 `  
` 🚧 === Breaking changes are highly expected === 🚧 `

## Features
* Vertical lists
* Multiple drop targets
* Mouse & Touch interactions

### TODO
- [ ] Horizontal lists
- [ ] Conditional drag & drop
- [ ] Auto-scroll container when dragging at container's edge
- [ ] Drag handlers
- [ ] Keyboard interactions
- [ ] Ignore form elements (opt-out)
- [ ] Scrollable containers
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
  type draggableId =
    | Todo(int);
  type droppableId =
    | TodoList;
};

module Todos = Dnd.Make(Cfg);

type state = {
  todos: array(Todo.t),
};

type action =
  | Reorder(Dnd.result(Cfg.draggableId, Cfg.droppableId));

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
    <Todos.Context onDrop=(result => Reorder(result) |> send)>
      ...(
           dnd =>
             <Todos.Droppable
               id=TodoList
               context=dnd.context
               className=(
                 (~draggingOver) =>
                   Cn.make(["todos", "active" |> Cn.ifSome(draggingOver)])
               )>
               (
                 state.todos
                 |. Array.map(todo =>
                      <Todos.Draggable
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
                        (todo.todo |> ReasonReact.string)
                      </Todos.Draggable>
                    )
                 |. ReasonReact.array
               )
             </Todos.Droppable>
         )
    </Todos.Context>,
};
```

## License

MIT.
