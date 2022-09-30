# Getting started

In this guide, we will build simple flat list with draggable items.

```
+----------------- CONTAINER ----------------+
|   +------------------------------------+   |
|   | Item #1                            |   |
|   +------------------------------------+   |
|   +------------------------------------+   |
|   | Item #2                            |   |
|   +------------------------------------+   |
|                    ...                     |
|   +------------------------------------+   |
|   | Item #N                            |   |
|   +------------------------------------+   |
+--------------------------------------------+
```

> In all guides, we use `Belt` as a standard library and it's always `open`ed. So keep in mind that in all code snippets `Belt` module is implicitly opened. I.e. `Array.map` is the same as `Belt.Array.map`.

First, let's shape up a state. This is going to be an array of ints:

```rescript
type item = int
type state = array<item>
```

To create components that handle all drag & drop business, we need to call `Dnd.Make()` functor:

```rescript
module Items = Dnd.Make(Item, Container)
```

This code wouldn't compile yet because we need to provide two modules to the `Dnd.Make` functor:

- `Item`: configuration for draggable item.
- `Container`: configuration for droppable container, which contains draggable items.

Both of these modules has the same signature:

```rescript
type t
let eq: (t, t) => bool
let cmp: (t, t) => int
```

Basically, functor asks you to answer the following questions:
1. What the thing is? _Answer: type `t`._
1. When two things given, do those equal or not? _Answer: `eq` function._
1. When two things given, how to compare those? _Answer: `cmp` function._

Let's start with very simple (and in general not 100% safe) implementation of `Item` container:

```rescript
module Item = {
  type t = item                  // `item` is a type alias we defined above which is resolved to `int`
  let eq = (x1, x2) => x1 == x2  // or more concise: let eq = (==)
  let cmp = compare              // default comparator from Pervasives module
}
```

Regarding `Container` type, there is no specific entity in the app domain which can be associated with this single abstract box that holds our flat list of items in UI. So we need to keep its configuration abstract, e.g.:

```rescript
module Container = {
  type t                                // abstract type
  external id: unit => t = "%identity"  // `Container.id()` would produce value of abstract type `t`
  let eq = (_, _) => true               // since `Container` is singleton, it's always equal to self
  let cmp = (_, _) => 0                 // same logic applies
}
```

For convenience, `rescript-dnd` exposes functor which would create such singleton for you, so you don't have to type this boilerplate yourself:

```rescript
module Container = Dnd.MakeSingletonContainer()
```

Now, when we have complete configuration defined, we can create module which holds React components:

```rescript
module Items = Dnd.Make(Item, Container)
```

Module `Items` holds 3 components (each link below leads to component's api):
- [`Items.DndManager`](./03-Api.md#dndmanager-component): component that manages drag & drop state.
- [`Items.DraggableItem`](./03-Api.md#draggableitem-component): component that is used to render draggable item.
- [`Items.DroppableContainer`](./03-Api.md#droppablecontainer-component): component that is used to render droppable container.

Let's render those:

```rescript
let (state, dispatch) = reducer->React.useReducer(initialState)

<Items.DndManager onReorder={result => ReorderItems(result)->dispatch}>
  <Items.DroppableContainer id={Container.id()} axis=Y>
    {state
    ->Array.mapWithIndex((index, item) =>
      <Items.DraggableItem
        id=item key={item->Int.toString} containerId={Container.id()} index>
        #Children(item->Int.toString->React.string)
      </Items.DraggableItem>
    )
    ->React.array}
  </Items.DroppableContainer>
</Items.DndManager>
```

Even though render tree looks good, to finish this component we still need to implement handler that would persist result of reordering when item gets dropped.

This is how corresponding `action` constructor type looks like:

```rescript
type action = ReorderItems(Dnd.result<Item.t, Container.t>)
```

What `Dnd.result` type is?

```rescript
type rec result<'item, 'container> = option<reorderResult<'item, 'container>>

and reorderResult<'item, 'container> =
  | SameContainer('item, placement<'item>)
  | NewContainer('item, 'container, placement<'item>)

and placement<'item> =
  | Before('item)
  | Last
```

Let's break down possible cases:

```rescript
| ReorderItems(None) =>
  // `None` means nothing has changed:
  // either user dropped the item on the same position
  // or pressed Esc key etc.

| ReorderItems(Some(SameContainer(item, Before(beforeItem)))) =>
  // `SameContainer` means that the `item` was dropped
  // onto the same container in which it was before the dragging.
  // `Before(beforeItem)` means it has landed in the position
  // before `beforeItem` in the list.
  // How new placement should be persisted is totally application concern.
  // `Dnd` only tells where the new placement is
  // relative to other elements in the list.

| ReorderItems(Some(SameContainer(item, Last))) =>
  // Similar to the previous branch,
  // but this time item has landed at the end of the list

| ReorderItems(Some(NewContainer(item, newContainer, placement))) =>
  // Same as `SameContainer`, but in this case
  // item was dropped onto the different container.
```

So with this in mind, let's implement reducer for our case:

```rescript
let reducer = (state, action) =>
  switch action {
  | ReorderItems(Some(SameContainer(item, placement))) =>
    // Item has landed in the new position of the same container,
    // so it should be reinserted from the old position
    // in the array into the new one.
    // `ArrayExt.reinsert` is a helper which does just this.
    state->ArrayExt.reinsert(
      ~value=item,
      ~place=switch placement {
      | Before(id) => #Before(id)
      | Last => #Last
      },
    )

  // not possible since we have only one container
  | ReorderItems(Some(NewContainer(_)))
  | ReorderItems(None) => state
  }
```

> `ArrayExt.reinsert` is not a part of the public API since usually in a real-world app reordering is handled differently. If you want to inspect it or use it in your own code, you can find its definition in the [examples](../examples/libs/ArrayExt.res).

Looks like we have everything in place. This is how the final module looks like:

```rescript
type item = int

module Item = {
  type t = item
  let eq = (x1, x2) => x1 == x2
  let cmp = compare
}

module Container = Dnd.MakeSingletonContainer()

module Items = Dnd.Make(Item, Container)

type state = array<item>

type action = ReorderItems(Dnd.result<Item.t, Container.t>)

let reducer = (state, action) =>
  switch action {
  | ReorderItems(Some(SameContainer(item, placement))) =>
    state->ArrayExt.reinsert(
      ~value=item,
      ~place=switch placement {
      | Before(id) => #Before(id)
      | Last => #Last
      },
    )
  | ReorderItems(Some(NewContainer(_)))
  | ReorderItems(None) => state
  }

let initialState = [1, 2, 3, 4, 5, 6, 7]

@react.component
let make = () => {
  let (state, dispatch) = reducer->React.useReducer(initialState)

  <Items.DndManager onReorder={result => ReorderItems(result)->dispatch}>
    <Items.DroppableContainer id={Container.id()} axis=Y>
      {state
      ->Array.mapWithIndex((index, item) =>
        <Items.DraggableItem
          id=item key={item->Int.toString} containerId={Container.id()} index>
          #Children(item->Int.toString->React.string)
        </Items.DraggableItem>
      )
      ->React.array}
    </Items.DroppableContainer>
  </Items.DndManager>
}
```

_Source code of the final module for this guide: [`GettingStartedGuide.res`](../examples/guides/GettingStartedGuide.res)_

---
This guide gives base overview of how `rescript-dnd` works. To find out more about how to make it safer and how to deal with multiple containers—proceed to the [next guide](./02-SaferIdentifiersAndMultipleContainersGuide.md).
