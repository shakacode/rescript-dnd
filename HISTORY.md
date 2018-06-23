# History

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
