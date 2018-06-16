module Cfg = {
  type draggableId =
    | Todo(int);
  type droppableId =
    | TodoList;
};

module Todos = Dnd.Make(Cfg);

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
  | Reorder(Dnd.result(Cfg.draggableId, Cfg.droppableId));

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~n, _) => {
  ...component,
  initialState: () => {
    todosIndex: Array.range(1, n),
    todosMap:
      Array.range(1, n)
      |. Array.reduceU(Map.Int.empty, (. map, id) =>
           map
           |. Map.Int.set(
                id,
                Todo.{id, todo: "Todo " ++ (id |. string_of_int)},
              )
         ),
  },
  reducer: (action, state) =>
    switch (action) {
    | Reorder(result) =>
      switch (result) {
      | SameTarget(Todo(_todoId), TodoList, todos) =>
        ReasonReact.Update({
          ...state,
          todosIndex:
            todos
            |. Array.map(todo =>
                 switch (todo) {
                 | Todo(id) => id
                 }
               ),
        })

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
                 Cn.make(["todos", "active" |> Cn.ifSome(dnd.target)])
               )>
               (
                 state.todosIndex
                 |. Array.mapU((. id) => {
                      let todo = state.todosMap |. Map.Int.getExn(id);

                      <Todos.Draggable
                        id=(Todo(todo.id))
                        key=(todo.id |. string_of_int)
                        droppableId=TodoList
                        context=dnd.context
                        className="todo">
                        (todo.todo |> ReasonReact.string)
                      </Todos.Draggable>;
                    })
                 |. ReasonReact.array
               )
             </Todos.Droppable>
         )
    </Todos.Context>,
};
