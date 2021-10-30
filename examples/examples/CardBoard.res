open Cx

module TodoId = Identity.Make()

module TodoListId = Identity.Make()

module Todo = {
  type t = {
    id: TodoId.t,
    title: string,
    todoListId: TodoListId.t,
  }
}

module TodoList = {
  type t = {
    id: TodoListId.t,
    title: string,
    todos: array<TodoId.t>,
  }
}

module Todos = {
  module Item = {
    type t = TodoId.t
    let eq = TodoId.eq
    let cmp = TodoId.cmp
  }

  module Container = {
    type t = TodoListId.t
    let eq = TodoListId.eq
    let cmp = TodoListId.cmp
  }

  include Dnd.Make(Item, Container)
}

module TodoLists = {
  module Item = {
    type t = TodoListId.t
    let eq = TodoListId.eq
    let cmp = TodoListId.cmp
  }

  module Container = Dnd.MakeSingletonContainer()

  include Dnd.Make(Item, Container)
}

type state = {
  todosMap: TodoId.Map.t<Todo.t>,
  todoListsMap: TodoListId.Map.t<TodoList.t>,
  todoListsIndex: array<TodoListId.t>,
}

type action =
  | ReorderTodos(Dnd.result<Todos.Item.t, Todos.Container.t>)
  | ReorderTodoLists(Dnd.result<TodoLists.Item.t, TodoLists.Container.t>)

let reducer = (state, action) =>
  switch action {
  | ReorderTodos(Some(SameContainer(todoId, placement))) =>
    let todo = state.todosMap->Map.getExn(todoId)
    {
      ...state,
      todoListsMap: state.todoListsMap->Map.update(todo.todoListId, x =>
        x->Option.map(list => {
          ...list,
          todos: list.todos->ArrayExt.reinsert(
            ~value=todoId,
            ~place=switch placement {
            | Before(id) => #Before(id)
            | Last => #Last
            },
          ),
        })
      ),
    }

  | ReorderTodos(Some(NewContainer(todoId, todoListId, placement))) =>
    let todo = state.todosMap->Map.getExn(todoId)
    {
      ...state,
      todosMap: state.todosMap->Map.update(todoId, x =>
        x->Option.map(todo => {...todo, todoListId: todoListId})
      ),
      todoListsMap: state.todoListsMap
      ->Map.update(todo.todoListId, x =>
        x->Option.map(list => {
          ...list,
          todos: list.todos->Array.keep(id => id->TodoId.toInt != todoId->TodoId.toInt),
        })
      )
      ->Map.update(todoListId, x =>
        x->Option.map(list => {
          ...list,
          todos: list.todos->ArrayExt.insert(
            ~value=todoId,
            ~place=switch placement {
            | Before(id) => #Before(id)
            | Last => #Last
            },
          ),
        })
      ),
    }

  | ReorderTodoLists(Some(SameContainer(todoListId, placement))) => {
      ...state,
      todoListsIndex: state.todoListsIndex->ArrayExt.reinsert(
        ~value=todoListId,
        ~place=switch placement {
        | Before(id) => #Before(id)
        | Last => #Last
        },
      ),
    }

  | ReorderTodoLists(Some(NewContainer(_)))
  | ReorderTodos(None)
  | ReorderTodoLists(None) => state
  }

@react.component
let make = () => {
  let initialState = React.useMemo0(() => {
    todoListsIndex: Array.range(1, 7)->TodoListId.array,
    todoListsMap: TodoListId.Map.make()
    ->Map.set(
      TodoListId.make(1),
      {
        open TodoList
        {
          id: TodoListId.make(1),
          title: "List #1",
          todos: Array.range(1, 4)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(2),
      {
        open TodoList
        {
          id: TodoListId.make(2),
          title: "List #2",
          todos: Array.range(5, 11)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(3),
      {
        open TodoList
        {
          id: TodoListId.make(3),
          title: "List #3",
          todos: Array.range(12, 14)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(4),
      {
        open TodoList
        {
          id: TodoListId.make(4),
          title: "List #4",
          todos: Array.range(15, 23)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(5),
      {
        open TodoList
        {
          id: TodoListId.make(5),
          title: "List #5",
          todos: Array.range(24, 28)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(6),
      {
        open TodoList
        {
          id: TodoListId.make(6),
          title: "List #6",
          todos: Array.range(29, 35)->TodoId.array,
        }
      },
    )
    ->Map.set(
      TodoListId.make(7),
      {
        open TodoList
        {
          id: TodoListId.make(7),
          title: "List #7",
          todos: Array.range(36, 40)->TodoId.array,
        }
      },
    ),
    todosMap: Array.range(1, 40)
    ->TodoId.array
    ->Array.reduce(TodoId.Map.make(), (map, id) =>
      map->Map.set(
        id,
        {
          open Todo
          {
            id: id,
            title: "Todo " ++ id->TodoId.toString,
            todoListId: {
              let id = id->TodoId.toInt
              if id >= 1 && id <= 4 {
                TodoListId.make(1)
              } else if id >= 5 && id <= 11 {
                TodoListId.make(2)
              } else if id >= 12 && id <= 14 {
                TodoListId.make(3)
              } else if id >= 15 && id <= 23 {
                TodoListId.make(4)
              } else if id >= 24 && id <= 28 {
                TodoListId.make(5)
              } else if id >= 29 && id <= 35 {
                TodoListId.make(6)
              } else {
                TodoListId.make(7)
              }
            },
          }
        },
      )
    ),
  })

  let (state, dispatch) = reducer->React.useReducer(initialState)

  <TodoLists.DndManager onReorder={result => ReorderTodoLists(result)->dispatch}>
    <Todos.DndManager onReorder={result => ReorderTodos(result)->dispatch}>
      <TodoLists.DroppableContainer
        id={TodoLists.Container.id()} axis=X className={(~draggingOver as _) => "todo-lists"}>
        {state.todoListsIndex
        ->Array.mapWithIndex((todoListIndex, todoListId) => {
          let todoList = state.todoListsMap->Map.getExn(todoListId)

          <TodoLists.DraggableItem
            id=todoListId
            key={todoListId->TodoListId.toString}
            containerId={TodoLists.Container.id()}
            index=todoListIndex
            className={(~dragging) => {
              cx(["todo-list", dragging ? "dragging" : ""])
            }}>
            #ChildrenWithDragHandle(
              (~style, ~onMouseDown, ~onTouchStart) =>
                <Todos.DroppableContainer
                  id=todoListId
                  key={todoListId->TodoListId.toString}
                  axis=Y
                  className={(~draggingOver) => {
                    cx(["todos", draggingOver ? "active" : ""])
                  }}>
                  <div className="todos-header">
                    <Control className="drag-handle" style onMouseDown onTouchStart>
                      <DragHandleIcon />
                    </Control>
                    <div className="title"> {todoList.title->React.string} </div>
                  </div>
                  {todoList.todos
                  ->Array.mapWithIndex((todoIndex, todoId) => {
                    let todo = state.todosMap->Map.getExn(todoId)

                    <Todos.DraggableItem
                      id=todoId
                      key={todoId->TodoId.toString}
                      containerId=todoListId
                      index=todoIndex
                      className={(~dragging) => {
                        cx(["todo", dragging ? "dragging" : ""])
                      }}>
                      #Children(todo.title->React.string)
                    </Todos.DraggableItem>
                  })
                  ->React.array}
                </Todos.DroppableContainer>,
            )
          </TodoLists.DraggableItem>
        })
        ->React.array}
      </TodoLists.DroppableContainer>
    </Todos.DndManager>
  </TodoLists.DndManager>
}
