# Api

## `Dnd.Make` functor
Functor that takes configurations for draggable item and droppable container and produces a module with React components.

```reason
module T: DndComponents = Dnd.Make(Item: DndEntry, Container: DndEntry);

module type DndEntry = {
  type t;
  let eq: (t, t) => bool;
};

module type DndComponents = {
  module type DndManager: DndManagerComponent;
  module type DraggableItem: DraggableItemComponent;
  module type DroppableContainer: DroppableContainerComponent;
};
```

## `DndManager` component
React component that manages drag & drop state.

```reason
let make: (
  ~onDragStart: option(hook)=?,
  ~onDropStart: option(hook)=?,
  ~onDropEnd: option(hook)=?,
  ~onReorder: option(result(Item.t, Container.t)) => unit,
  ~children: React.element,
) => React.element;

type hook = (~itemId: Item.t) => unit;

type result('item, 'container) = option(reorderResult('item, 'container))

and reorderResult('item, 'container) =
  | SameContainer('item, placement('item))
  | NewContainer('item, 'container, placement('item))

and placement('item) =
  | Before('item)
  | Last;
```

## `DraggableItem` component
React component that is used to render draggable item.

```reason
let make: (
  ~id: Item.t,
  ~containerId: Container.t,
  ~index: int,
  ~className: option((~dragging: bool) => string)=?,
  ~children: React.element,
) => React.element;
```

## `DroppableContainer` component
React component that is used to render droppable container.

```reason
let make: (
  ~id: Container.t,
  ~axis: axis,
  ~lockAxis: bool=false,
  ~accept: option(Item.t => bool)=?,
  ~className: option((~draggingOver: bool) => string)=?,
  ~children: React.element,
) => React.element;

type axis =
  | X
  | Y;
```

## `Dnd.MakeSingletonContainer` functor
For convenience, `re-dnd` exposes this functor which would create `Container` module when there is no specific entity in the app domain which can be associated with container that holds some single list of items in UI.

```reason
module Container: DndEntry = Dnd.MakeSingletonContainer();
```
