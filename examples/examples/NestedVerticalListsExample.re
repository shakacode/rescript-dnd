open Dnd__React;

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
      | TodoListContainer(int);

    let eq = (d1, d2) =>
      switch (d1, d2) {
      | (TodoListContainer(id1), TodoListContainer(id2)) => id1 === id2
      };
  };
};

module Todos = Dnd.Make(Cfg);

module Todo = {
  type t = {
    id: int,
    todo: string,
  };
};

module TodoList = {
  type t = {
    id: int,
    name: string,
    todos: array(int),
  };
};

type state = {
  todoListsIndex: array(int),
  todoListsMap: Map.Int.t(TodoList.t),
  todosMap: Map.Int.t(Todo.t),
};

type action =
  | Reorder(Dnd.result(Cfg.Draggable.t, Cfg.Droppable.t));

let component = ReasonReact.reducerComponent(__MODULE__);

let make = _ => {
  ...component,
  initialState: () => {
    todoListsIndex: Array.range(1, 2),
    todoListsMap:
      Map.Int.empty
      |. Map.Int.set(
           1,
           TodoList.{id: 1, name: "First list", todos: Array.range(1, 5)},
         )
      |. Map.Int.set(
           2,
           TodoList.{id: 2, name: "Second list", todos: Array.range(6, 10)},
         ),
    todosMap:
      Array.range(1, 10)
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
      | SameTarget(Todo(_todoId), TodoListContainer(listId), todos) =>
        ReasonReact.Update({
          ...state,
          todoListsMap:
            state.todoListsMap
            |. Map.Int.update(
                 listId,
                 list => {
                   let list = list |. Option.getExn;
                   Some({
                     ...list,
                     todos:
                       todos
                       |. Array.map(todo =>
                            switch (todo) {
                            | Todo(id) => id
                            }
                          ),
                   });
                 },
               ),
        })

      | NewTarget(
          Todo(todoId),
          {
            prev: TodoListContainer(originalListId),
            next: TodoListContainer(targetListId),
          },
          todos,
        ) =>
        ReasonReact.Update({
          ...state,
          todoListsMap:
            state.todoListsMap
            |. Map.Int.update(
                 originalListId,
                 list => {
                   let list = list |. Option.getExn;
                   Some({
                     ...list,
                     todos: list.todos |. Array.keep(id => id !== todoId),
                   });
                 },
               )
            |. Map.Int.update(
                 targetListId,
                 list => {
                   let list = list |. Option.getExn;
                   Some({
                     ...list,
                     todos:
                       todos
                       |. Array.map(todo =>
                            switch (todo) {
                            | Todo(id) => id
                            }
                          ),
                   });
                 },
               ),
        })

      | NoChanges => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) =>
    <Todos.Context onDrop=(result => Reorder(result) |> send)>
      ...(
           dnd =>
             <Fragment>
               (
                 state.todoListsIndex
                 |. Array.mapU((. listId) => {
                      let list = state.todoListsMap |. Map.Int.getExn(listId);

                      <Todos.Droppable
                        id=(TodoListContainer(list.id))
                        key=(list.id |. string_of_int)
                        context=dnd.context
                        className=(
                          (~draggingOver) =>
                            Cn.make([
                              "todos",
                              "active" |> Cn.ifTrue(draggingOver),
                            ])
                        )>
                        (
                          list.todos
                          |. Array.mapU((. id) => {
                               let todo =
                                 state.todosMap |. Map.Int.getExn(id);

                               <Todos.Draggable
                                 id=(Todo(todo.id))
                                 key=(todo.id |. string_of_int)
                                 droppableId=(TodoListContainer(list.id))
                                 context=dnd.context
                                 className=(
                                   (~dragging) =>
                                     Cn.make([
                                       "todo",
                                       "dragging" |> Cn.ifTrue(dragging),
                                     ])
                                 )>
                                 (todo.todo |> ReasonReact.string)
                               </Todos.Draggable>;
                             })
                          |. ReasonReact.array
                        )
                      </Todos.Droppable>;
                    })
                 |. ReasonReact.array
               )
             </Fragment>
         )
    </Todos.Context>,
};
