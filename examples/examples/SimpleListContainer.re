open Dnd__React;

module Cfg = {
  module Draggable = {
    type t =
      | Todo(int);

    let eq = (d1, d2) =>
      switch (d1, d2) {
      | (Todo(id1), Todo(id2)) => id1 == id2
      };
  };

  module Droppable = {
    type t =
      | TodosDroppable;

    let eq = (_, _) => true;
  };
};

module Screen = Dnd.Make(Cfg);

module Todo = {
  type t = {
    id: int,
    todo: string,
  };
};

type state = {
  todosIndex: array(int),
  todosMap: Map.Int.t(Todo.t),
};

type action =
  | Reorder(Dnd.result(Cfg.Draggable.t, Cfg.Droppable.t));

let component = React.reducerComponent(__MODULE__);
let make = (~n, ~layout, _) => {
  ...component,
  initialState: () => {
    todosIndex: Array.range(1, n),
    todosMap:
      Array.range(1, n)
      ->Array.reduceU(Map.Int.empty, (. map, id) =>
          map->Map.Int.set(id, Todo.{id, todo: "Todo " ++ id->string_of_int})
        ),
  },
  reducer: (action, state) =>
    switch (action) {
    | Reorder(result) =>
      switch (result) {
      | SameTarget(Todo(_id), TodosDroppable, todos) =>
        React.Update({
          ...state,
          todosIndex:
            todos->Array.map(
              fun
              | Todo(id) => id,
            ),
        })

      | NewTarget(_, _, _)
      | NoChanges => React.NoUpdate
      }
    },
  render: ({state, send}) =>
    <Screen.Context onDrop={result => Reorder(result)->send}>
      ...{dnd =>
        <Screen.Droppable
          id=TodosDroppable
          axis={
            switch (layout) {
            | Example.Horizontal => X
            | Example.Vertical => Y
            | Example.CardBoard =>
              failwith("Don't use CardBoard layout with SimpleList")
            }
          }
          context={dnd.context}
          className={(~draggingOver) =>
            Cn.make(["todos", "active"->Cn.ifTrue(draggingOver)])
          }>
          {state.todosIndex
           ->Array.mapWithIndexU((. index, id) => {
               let todo = state.todosMap->Map.Int.getExn(id);

               <Screen.Draggable
                 id={Todo(todo.id)}
                 key={todo.id->string_of_int}
                 droppableId=TodosDroppable
                 index
                 context={dnd.context}
                 className={(~dragging, ~moving) =>
                   Cn.make([
                     "todo",
                     "dragging"->Cn.ifTrue(dragging),
                     "moving"->Cn.ifTrue(moving),
                   ])
                 }>
                 ...{Children(todo.todo->React.string)}
               </Screen.Draggable>;
             })
           ->React.array}
        </Screen.Droppable>
      }
    </Screen.Context>,
};
