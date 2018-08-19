module Cfg = {
  module Draggable = {
    type t =
      | Todo(int)
      | TodoList(int);

    let eq = (d1, d2) =>
      switch (d1, d2) {
      | (Todo(id1), Todo(id2)) => id1 === id2
      | (TodoList(id1), TodoList(id2)) => id1 === id2
      | _ => false
      };
  };

  module Droppable = {
    type t =
      | TodosDroppable(int)
      | TodoListsDroppable;

    let eq = (d1, d2) =>
      switch (d1, d2) {
      | (TodosDroppable(id1), TodosDroppable(id2)) => id1 === id2
      | (TodoListsDroppable, TodoListsDroppable) => true
      | _ => false
      };
  };
};

module Screen = Dnd.Make(Cfg);

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
    todoListsIndex: Array.range(1, 7),
    todoListsMap:
      Map.Int.empty
      |. Map.Int.set(
           1,
           TodoList.{id: 1, name: "List #1", todos: Array.range(1, 4)},
         )
      |. Map.Int.set(
           2,
           TodoList.{id: 2, name: "List #2", todos: Array.range(5, 11)},
         )
      |. Map.Int.set(
           3,
           TodoList.{id: 3, name: "List #3", todos: Array.range(12, 14)},
         )
      |. Map.Int.set(
           4,
           TodoList.{id: 4, name: "List #4", todos: Array.range(15, 23)},
         )
      |. Map.Int.set(
           5,
           TodoList.{id: 5, name: "List #5", todos: Array.range(24, 28)},
         )
      |. Map.Int.set(
           6,
           TodoList.{id: 6, name: "List #6", todos: Array.range(29, 35)},
         )
      |. Map.Int.set(
           7,
           TodoList.{id: 7, name: "List #7", todos: Array.range(36, 40)},
         ),
    todosMap:
      Array.range(1, 40)
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
      /* Handle TodoLists reordering */
      | SameTarget(TodoList(_id), TodoListsDroppable, lists) =>
        ReasonReact.Update({
          ...state,
          todoListsIndex:
            lists
            |. Array.map(
                 fun
                 | TodoList(id) => id
                 | _ => failwith("Draggable is not TodoList"),
               ),
        })

      /* Handle Todos reordering within same TodoList */
      | SameTarget(Todo(_id), TodosDroppable(listId), todos) =>
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
                       |. Array.map(
                            fun
                            | Todo(id) => id
                            | _ => failwith("Draggable is not Todo"),
                          ),
                   });
                 },
               ),
        })

      /* Handle Todos reordering when Todo is moved to the new TodoList */
      | NewTarget(
          Todo(todoId),
          {
            prev: TodosDroppable(originalListId),
            next: TodosDroppable(targetListId),
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
                       |. Array.map(
                            fun
                            | Todo(id) => id
                            | _ => failwith("Draggable is not Todo"),
                          ),
                   });
                 },
               ),
        })

      | NoChanges
      | SameTarget(Todo(_), TodoListsDroppable, _)
      | SameTarget(TodoList(_), TodosDroppable(_), _)
      | NewTarget(Todo(_), _, _)
      | NewTarget(TodoList(_), _, _) => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) =>
    <Screen.Context onDrop=(result => Reorder(result) |> send)>
      ...(
           dnd =>
             <Screen.Droppable
               id=TodoListsDroppable
               axis=X
               accept=(
                        fun
                        | Todo(_) => false
                        | TodoList(_) => true
                      )
               context=dnd.context
               className=((~draggingOver as _) => "todo-lists")>
               (
                 state.todoListsIndex
                 |. Array.mapWithIndexU((. index, listId) => {
                      let list = state.todoListsMap |. Map.Int.getExn(listId);

                      <Screen.Draggable
                        id=(TodoList(list.id))
                        key=(list.id |. string_of_int)
                        droppableId=TodoListsDroppable
                        index
                        context=dnd.context
                        className=(
                          (~dragging, ~moving) =>
                            Cn.make([
                              "todo-list",
                              "dragging" |> Cn.ifTrue(dragging),
                              "moving" |> Cn.ifTrue(moving),
                            ])
                        )>
                        ...(
                             ChildrenWithDragHandle(
                               handle =>
                                 <Screen.Droppable
                                   id=(TodosDroppable(list.id))
                                   key=(list.id |. string_of_int)
                                   axis=Y
                                   accept=(
                                            fun
                                            | Todo(_) => true
                                            | TodoList(_) => false
                                          )
                                   context=dnd.context
                                   className=(
                                     (~draggingOver) =>
                                       Cn.make([
                                         "todos",
                                         "active" |> Cn.ifTrue(draggingOver),
                                       ])
                                   )>
                                   <div className="todos-header">
                                     <Control
                                       className="drag-handle"
                                       style=handle.style
                                       onKeyDown=handle.onKeyDown
                                       onMouseDown=handle.onMouseDown
                                       onTouchStart=handle.onTouchStart>
                                       <DragHandleIcon />
                                     </Control>
                                     <div className="title">
                                       (list.name |> ReasonReact.string)
                                     </div>
                                   </div>
                                   (
                                     list.todos
                                     |. Array.mapWithIndexU((. index, id) => {
                                          let todo =
                                            state.todosMap
                                            |. Map.Int.getExn(id);

                                          <Screen.Draggable
                                            id=(Todo(todo.id))
                                            key=(todo.id |. string_of_int)
                                            droppableId=(
                                              TodosDroppable(list.id)
                                            )
                                            index
                                            context=dnd.context
                                            className=(
                                              (~dragging, ~moving) =>
                                                Cn.make([
                                                  "todo",
                                                  "dragging"
                                                  |> Cn.ifTrue(dragging),
                                                  "moving"
                                                  |> Cn.ifTrue(moving),
                                                ])
                                            )>
                                            ...(
                                                 Children(
                                                   todo.todo
                                                   |> ReasonReact.string,
                                                 )
                                               )
                                          </Screen.Draggable>;
                                        })
                                     |. ReasonReact.array
                                   )
                                 </Screen.Droppable>,
                             )
                           )
                      </Screen.Draggable>;
                    })
                 |. ReasonReact.array
               )
             </Screen.Droppable>
         )
    </Screen.Context>,
};
