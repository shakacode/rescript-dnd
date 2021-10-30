module MakeId = () => {
  module Id: {
    type t
  } = {
    type t = int
  }

  type t = Id.t
  external make: int => t = "%identity"
  external toInt: t => int = "%identity"
  let toString = x => x->toInt->Int.toString

  let eq = (x1, x2) => x1->toInt == x2->toInt
  let cmp = (x1, x2) => Pervasives.compare(x1->toInt, x2->toInt)

  module Comparable = Belt.Id.MakeComparable({
    type t = Id.t
    let cmp = cmp
  })

  module Map = {
    type t<'t> = Map.t<Id.t, 't, Comparable.identity>
    let make = () => Map.make(~id=module(Comparable))
  }
}

module TodoId = MakeId()

module TodoListId = MakeId()

module DraggableItem = {
  type t = TodoId.t
  let eq = TodoId.eq
  let cmp = TodoId.cmp
}

module DroppableContainer = {
  type t = TodoListId.t
  let eq = TodoListId.eq
  let cmp = TodoListId.cmp
}

module Todos = Dnd.Make(DraggableItem, DroppableContainer)

type todo = {
  id: TodoId.t,
  title: string,
  todoListId: TodoListId.t,
}

type todoList = {
  id: TodoListId.t,
  title: string,
  todos: array<TodoId.t>,
}

type state = {
  todoListIndex: array<TodoListId.t>,
  todoListMap: TodoListId.Map.t<todoList>,
  todoMap: TodoId.Map.t<todo>,
}

type action = ReorderTodos(Dnd.result<DraggableItem.t, DroppableContainer.t>)

let reducer = (state, action) =>
  switch action {
  | ReorderTodos(Some(SameContainer(todoId, placement))) =>
    let todo = state.todoMap->Map.getExn(todoId)
    {
      ...state,
      todoListMap: // Updating todos index of todo list
      state.todoListMap->Map.update(todo.todoListId, todoList =>
        todoList->Option.map(todoList => {
          ...todoList,
          todos: todoList.todos->ArrayExt.reinsert(
            ~value=todoId,
            ~place=switch placement {
            | Before(id) => #Before(id)
            | Last => #Last
            },
          ),
        })
      ),
    }

  | ReorderTodos(Some(NewContainer(todoId, targetTodoListId, placement))) =>
    let todo = state.todoMap->Map.getExn(todoId)
    {
      ...state,
      todoMap: // Updating todoListId of dropped todo
      state.todoMap->Map.update(todoId, todo =>
        todo->Option.map(todo => {...todo, todoListId: targetTodoListId})
      ),
      todoListMap: state.todoListMap
      // Removing todoId from old todo list index
      // since todo was dropped onto another container
      ->Map.update(todo.todoListId, todoList =>
        todoList->Option.map(todoList => {
          ...todoList,
          todos: todoList.todos->Array.keep(todoId' =>
            todoId'->TodoId.toInt != todoId->TodoId.toInt
          ),
        })
      )
      // Inserting todoId into the todos index of the target todo list
      ->Map.update(targetTodoListId, todoList =>
        todoList->Option.map(todoList => {
          ...todoList,
          todos: todoList.todos->ArrayExt.insert(
            ~value=todoId,
            ~place=switch placement {
            | Before(id) => #Before(id)
            | Last => #Last
            },
          ),
        })
      ),
    }

  | ReorderTodos(None) => state
  }

let initialState = {
  todoListIndex: [TodoListId.make(1), TodoListId.make(2)],
  todoListMap: TodoListId.Map.make()
  ->Map.set(
    TodoListId.make(1),
    {
      id: TodoListId.make(1),
      title: "Todo list #1",
      todos: [TodoId.make(1), TodoId.make(2)],
    },
  )
  ->Map.set(
    TodoListId.make(2),
    {
      id: TodoListId.make(2),
      title: "Todo list #2",
      todos: [TodoId.make(3), TodoId.make(4)],
    },
  ),
  todoMap: TodoId.Map.make()
  ->Map.set(
    TodoId.make(1),
    {
      id: TodoId.make(1),
      title: "Todo #1",
      todoListId: TodoListId.make(1),
    },
  )
  ->Map.set(
    TodoId.make(2),
    {
      id: TodoId.make(2),
      title: "Todo #2",
      todoListId: TodoListId.make(1),
    },
  )
  ->Map.set(
    TodoId.make(3),
    {
      id: TodoId.make(3),
      title: "Todo #3",
      todoListId: TodoListId.make(2),
    },
  )
  ->Map.set(
    TodoId.make(4),
    {
      id: TodoId.make(4),
      title: "Todo #4",
      todoListId: TodoListId.make(2),
    },
  ),
}

@react.component
let make = () => {
  let (state, dispatch) = reducer->React.useReducer(initialState)

  <Todos.DndManager onReorder={result => ReorderTodos(result)->dispatch}>
    {state.todoListIndex
    ->Array.map(todoListId => {
      let todoList = state.todoListMap->Map.getExn(todoListId)

      <Todos.DroppableContainer id=todoListId axis=Y key={todoListId->TodoListId.toString}>
        <h2> {todoList.title->React.string} </h2>
        {todoList.todos
        ->Array.mapWithIndex((index, todoId) => {
          let todo = state.todoMap->Map.getExn(todoId)

          <Todos.DraggableItem id=todoId key={todoId->TodoId.toString} containerId=todoListId index>
            #Children(todo.title->React.string)
          </Todos.DraggableItem>
        })
        ->React.array}
      </Todos.DroppableContainer>
    })
    ->React.array}
  </Todos.DndManager>
}
