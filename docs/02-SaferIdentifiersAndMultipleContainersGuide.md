# Safer identifiers and multiple containers
In this guide, we will build a UI with multiple todo lists, each contains todos that can be reordered within a list, as well as dragged between the lists.

Most of the topics here are more advanced (relative to [Getting Started](./01-GettingStartedGuide.md) guide), though these are mostly not specific to the `rescript-dnd`.

## Safe identifiers
Pretty much each entity in our apps has special field that uniquely identifies it. Usually, it's called `id`. Type of such identifier can be `int` or `string` (or any other serializable data type). But when we deal with identifiers of these loose types, there are no guarantees that entity of identifier is not confused with identifier of another entity or even some arbitrary `int` or `string`. In some cases, like handling of nested lists, it can cause nasty bugs that won't be caught by compiler. But this is fixable.

Consider `todo` entity of the following type:

```rescript
type todo = {
  id: int,
  title: string,
}
```

As a first step to make its identifier safer, let's create `TodoId` module and move type of `todo.id` to this module:

```rescript
module TodoId = {
  type t = int
}

type todo = {
  id: TodoId.t,
  title: string,
}
```

With this change, we didn't gain any additional safety yet since compiler still resolves the type of identifier to `int`. Basically, we just aliased it in our code but nothing has changed for compiler. To gain extra safety, we must hide underlying type of `TodoId.t` from the rest of the app and make this type _opaque_.

```rescript
module TodoId: { type t } = {
  type t = int
}
```

Spot the difference: in this version we annotated `TodoId` module. And within this annotation, type `t` doesn't have any special type assigned, it is opaque to the rest of the app. Though inside the module it is still aliased to `int`, but only internals of `TodoId` module are aware of it.

How it affects a program flow:

```rescript
// Before
let x = todo.id + 1 // Compiles

// After
let x = todo.id + 1 // Error: This expression has type TodoId.t but an expression was expected of type int
```

Now we have compile time guarantees that todo identifier can never be confused with any other identifier or random `int`.

As we will have to be dealing with conversion of raw value of identifier (from int or json or whatever) to the opaque type and back, `TodoId` module is going to contain such functions as `make`, `toInt`, `toString`, `fromJson`, `toJson`. Let's implement `make` function and restructure `TodoId` module a bit so we don't have to annotate all of its content.

```rescript
module TodoId = {
  module Id: {
    type t
  } = {
    type t = int
  }

  type t = Id.t
  external make: int => t = "%identity"
}
```

What we did here is hidden implementation of todo identifier in `Id` submodule and implemented `make` function which casts `int` to `t` using `"%identity"` external. Now, `TodoId.make(1)` would produce entity of `TodoId.t` type.

As a side note, `make` function doesn't have any runtime footprint and gets erased during compilation. It exists exclusively for compiler and you get compile-time safety with no runtime cost.

## Shaping up the state
Now, when we have safe identifiers, let's get back to UI and shape up the state of it. So, what we have here is `todoLists` and `todos` that belong to `todoLists`. The data that comes from a server (or any other source) are usually denormalized and looks similar to this:

```
{
  todoLists: [
    {
      id: 1,
      title: "Todo list #1",
      todos: [
        {
          id: 1,
          title: "Todo #1",
          todoListId: 1,
        },
        {
          id: 2,
          title: "Todo #2",
          todoListId: 1,
        },
      ],
    },
    {
      id: 2,
      title: "Todo list #2",
      todos: [
        {
          id: 3,
          title: "Todo #3",
          todoListId: 2,
        },
        {
          id: 4,
          title: "Todo #4",
          todoListId: 2,
        },
      ],
    },
  ],
}
```

Before storing data in a client state, we can _normalize_ data to the following shape.

```
{
  todoListIndex: [1, 2],
  todoListMap: {
    1: {
      id: 1,
      title: "Todo list #1",
      todos: [1, 2],
    },
    2: {
      id: 2,
      title: "Todo list #2",
      todos: [3, 4],
    },
  },
  todoMap: {
    1: {
      id: 1,
      title: "Todo #1",
      todoListId: 1,
    },
    2: {
      id: 2,
      title: "Todo #2",
      todoListId: 1,
    },
    3: {
      id: 3,
      title: "Todo #3",
      todoListId: 2,
    },
    4: {
      id: 4,
      title: "Todo #4",
      todoListId: 2,
    },
  }
}
```

The main point of this transformation is to have indices of entities as an ordered collections of identifiers and dictionaries of entities with keys as identifiers + values as entities themself. This way it is easier to update nested entities and manage reordering of things in UI.

> How to perform normalization is out of the scope of this guide.
> At Minima, we write normalizers by hand. It's usually 10-20 LOC each.

Let's describe normalized data in Reason types.

Indices are going to be arrays, i.e.:

```rescript
type todoListIndex = array<TodoListId.t>
type todoIndex = array<TodoId.t>
```

And for the dictionaries we're going to be using `Map` provided by the `Belt`. Here, things get a little bit tricky. `Belt` has built-in `Map.Int.t` but since our identifiers are opaque types now, it's not useful for our case. We need to prepare our own `Map.t` for each kind of identifier by extending `TodoId` and `TodoListId` modules.

```rescript
module Id: {
  type t
} = {
  type t = int
}

type t = Id.t
external make: int => t = "%identity"

module Comparable = Belt.Id.MakeComparable({
  type t = Id.t
  let cmp = Pervasives.compare
})

module Map = {
  type t<'t> = Map.t<Id.t, 't, Comparable.identity>
  let make = () => Map.make(~id=module(Comparable))
}
```

With all types in place, we can finally shape the state:

```rescript
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
  todoMap: TodoId.Map.t<todo>,
  todoListMap: TodoListId.Map.t<todoList>,
}
```

## Reducing boilerplate
Since we need to create own module for each kind of identifier, there will be 2 identical modules: `TodoId` and `TodoListId`. It's pretty boilerplate'y, but we can abstract creation of module for each identifier into a functor:

```rescript
module MakeId = () => {
  module Id: {
    type t
  } = {
    type t = int
  }

  type t = Id.t
  external make: int => t = "%identity"
  external toInt: t => int = "%identity"

  let eq = (x1, x2) => x1->toInt == x2->toInt
  let cmp = (x1, x2) => compare(x1->toInt, x2->toInt)

  module Comparable = Belt.Id.MakeComparable({
    type t = Id.t
    let cmp = cmp
  })

  module Map = {
    type t<'t> = Map.t<Id.t, 't, Comparable.identity>
    let make = () => Map.make(~id=module(Comparable))
  }
}
```

And then to create a module for each identifier call it without arguments:

```rescript
module TodoId = MakeId()
module TodoListId = MakeId()
```

## Drag & drop
With all the hard prerequisite work done, we can finally proceed to implementation of drag & drop functionality. The same way as we did in the [Getting Started](./01-GettingStartedGuide.md) guide, we need to define configuration modules to create DnD components.

```rescript
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
```

Using created components, we can shape a render tree:

```rescript
<Todos.DndManager onReorder={result => ReorderTodos(result)->dispatch}>
  {state.todoListIndex
  ->Array.map(todoListId => {
    let todoList = state.todoListMap->Map.getExn(todoListId)

    <Todos.DroppableContainer
      id=todoListId axis=Y key={todoListId->TodoListId.toString}>
      <h2> {todoList.title->React.string} </h2>
      {todoList.todos
      ->Array.mapWithIndex((index, todoId) => {
        let todo = state.todoMap->Map.getExn(todoId)

        <Todos.DraggableItem
          id=todoId key={todoId->TodoId.toString} containerId=todoListId index>
          #Children(todo.title->React.string)
        </Todos.DraggableItem>
      })
      ->React.array}
    </Todos.DroppableContainer>
  })
  ->React.array}
</Todos.DndManager>
```

The last touch is to implement persistence logic as following:
1. If todo was dropped to the new position within the same todo list, reinsert its id in `todoList.todos` array.
2. If todo was dropped onto another todo list, then
  - update `todo.todoListId` value
  - remove its id from previous `todoList.todos` array
  - and insert it into the target `todoList.todos` array

Let's implement it in a reducer:

```rescript
let reducer = (state, action) =>
  switch action {
  | ReorderTodos(Some(SameContainer(todoId, placement))) =>
    let todo = state.todoMap->Map.getExn(todoId)
    {
      ...state,
      todoListMap: 
        // Updating todos index of todo list
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
      todoMap: 
        // Updating todoListId of dropped todo
        state.todoMap->Map.update(todoId, todo =>
          todo->Option.map(todo => {...todo, todoListId: targetTodoListId})
        ),
      todoListMap: 
        state.todoListMap
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
```

_Source code of the final module for this guide: [`SaferIdentifiersAndMultipleContainersGuide.res`](../examples/guides/SaferIdentifiersAndMultipleContainersGuide.res)_

---
Phew, this was a long read! If you're interested in more examples, checkout [live demo](https://rescript-dnd.vercel.app/) and its [sources](../examples).

Or you can explore [API doc](./03-Api.md).
