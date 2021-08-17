# History

## 5.0.0
* **[ BREAKING ]** Switch to `@rescript/react`.
* **[ BREAKING ]** Switch to `rescript` from `bs-platform`.
* **[ BREAKING ]** Drop support for `rescript-logger`

## 4.0.0-beta.1
* **[ BREAKING ]** Switch to `@rescript/react`.
* **[ BREAKING ]** Update `bs-platform`.

## 3.0.0
* **[ BREAKING ]** Replace `bs-log` with `res-logger`.
* **[ BREAKING ]** Move `bs-webapi` to peer dependensies.

## 2.0.0
* **[ BREAKING ]** Modules of `DndEntry` type must provide `cmp` function to avoid polymorphic comparison. See [this diff](https://github.com/MinimaHQ/re-dnd/commit/238a0abab8ad88a7c643c0b022c49887c025a89b) for details.
* **[ BREAKING ]** Minimum required version of `bs-platform` is `7.1.1`.
* **[ BREAKING ]** Minimum required version of `reason-react` is `0.8.0`.

## 1.2.0
* `bs-platform` updated to `v7`.
* `bs-log` updated to `v1`.

## 1.1.0
* Added `onDragStart`, `onDropStart` & `onDropEnd` handlers.

## 1.0.0
* Full rewrite using hooks api.

## 0.6.0
**Features**
* Horizontal lists support.

## 0.5.0
**Features**
* Scrollable containers support.

## 0.4.0
**Features**
* Auto-scroll at the vertical edges of the window.

## 0.3.1
**Fixes**
* Fix sorting for case when draggable that being dragged is way bigger or smaller than siblings.
* Fix determination of a landing point when dropping on empty droppable with header.

## 0.3.0
**Features**
* Conditional drag & drop. Now each `Droppable` takes optional `accept` prop:

```reason
~accept: option(Draggable.t => bool)=?

<Dnd.Droppable
  accept=(
    fun
    | Todo => true
    | TodoList => false
  )
/>
```

* Custom drag handles.

```reason
/* Without custom drag handle */
<Dnd.Draggable>
  ...(Children("Drag me" |> ReasonReact.string))
</Dnd.Draggable>

/* With custom drag handle */
<Dnd.Draggable>
  ...(
    ChildrenWithHandle(
      handle =>
        <button
          style=handle.style
          onMouseDown=handle.onMouseDown
          onTouchStart=handle.onTouchStart
        >
          ("Drag me" |> ReasonReact.string)
        </button>
    )
  )
</Dnd.Draggable>
```

## 0.2.0
**API**
* **[ BREAKING ]** `Config` updated: structure is changed and `eq` function is required for both types `Draggable.t` and `Droppable.t`. It should improve overall perf.

```diff
- module type Config = {
-   type draggableId;
-   type droppableId;
- };

+ module type Config = {
+   module Draggable: {
+     type t;
+     let eq: (t, t) => bool;
+   };
+
+   module Droppable: {
+     type t;
+     let eq: (t, t) => bool;
+   };
+ };
```

* **[ BREAKING ]** `Droppable`'s '`className` function receives `~draggingOver: bool` instead of `~draggingOver: option('droppableId)` (by virtue of the first change).

```diff
- type className = (~draggingOver: option('droppableId)) => string;
+ type className = (~draggingOver: bool) => string;
```

## 0.1.1
**Improvements**
* Don't interrupt text selection on desktops.
* Disable text selection on drag in Safari.

## 0.1.0
**Features**
* Add touch interactions support.
* Make `className` prop a function (`Draggable` & `Droppable`).

## 0.0.1
* Core architecture.

**Features**
* Vertical lists.
* Multiple drop targets.
