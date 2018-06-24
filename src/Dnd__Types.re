module Point = {
  type t = {
    x: int,
    y: int,
  };
};

module Delta = {
  type t = {
    x: int,
    y: int,
  };
};

module Dimensions = {
  type t = {
    width: int,
    height: int,
  };
};

module Rect = {
  type t = {
    top: int,
    bottom: int,
    left: int,
    right: int,
  };
};

module Margins = {
  type t = {
    top: int,
    bottom: int,
    left: int,
    right: int,
  };
};

module Borders = {
  type t = {
    top: int,
    bottom: int,
    left: int,
    right: int,
  };
};

module Paddings = {
  type t = {
    top: int,
    bottom: int,
    left: int,
    right: int,
  };
};

module Offset = {
  type t = {
    top: int,
    bottom: int,
    left: int,
    right: int,
  };
};

module Direction = {
  type t =
    | Alpha
    | Omega;
};

module Geometry = {
  type t = {
    rect: Rect.t,
    dimensions: Dimensions.t,
    margins: Margins.t,
    borders: Borders.t,
    paddings: Paddings.t,
  };
};

module DraggableBag = {
  type t('draggableId, 'droppableId) = {
    id: 'draggableId,
    droppableId: 'droppableId,
    element: Dom.htmlElement,
    geometry: option(Geometry.t),
    shift: option(Direction.t),
    animating: bool,
  };

  type className = (~dragging: bool) => string;
};

module DroppableBag = {
  type t('draggableId, 'droppableId) = {
    id: 'droppableId,
    element: Dom.htmlElement,
    geometry: option(Geometry.t),
    accept: option('draggableId => bool),
  };

  type className = (~draggingOver: bool) => string;
};

module Ghost = {
  type t('draggableId, 'droppableId) = {
    draggableId: 'draggableId,
    originalDroppable: 'droppableId,
    targetDroppable: option('droppableId),
    targetingOriginalDroppable: bool,
    direction: Direction.t,
    dimensions: Dimensions.t,
    margins: Margins.t,
    borders: Borders.t,
    delta: Delta.t,
    center: Point.t,
    departurePoint: Point.t,
    currentPoint: Point.t,
    departureRect: Rect.t,
    currentRect: Rect.t,
  };
};

module Subscriptions = {
  type t = {
    install: unit => unit,
    drop: unit => unit,
  };
};

module Status = {
  type t('draggableId, 'droppableId) =
    | StandBy
    | Dragging(Ghost.t('draggableId, 'droppableId), Subscriptions.t)
    | Dropping(Ghost.t('draggableId, 'droppableId));
};

module Context = {
  type t('draggableId, 'droppableId) = {
    status: Status.t('draggableId, 'droppableId),
    target: option('droppableId),
    registerDraggable:
      (('draggableId, 'droppableId, Dom.htmlElement)) => unit,
    registerDroppable:
      (('droppableId, option('draggableId => bool), Dom.htmlElement)) => unit,
    disposeDraggable: 'draggableId => unit,
    disposeDroppable: 'droppableId => unit,
    getDraggableShift: 'draggableId => option(Direction.t),
    startDragging:
      (
        ~draggableId: 'draggableId,
        ~droppableId: 'droppableId,
        ~start: Point.t,
        ~current: Point.t,
        ~element: Dom.htmlElement,
        ~subscriptions: Subscriptions.t
      ) =>
      unit,
    updateGhostPosition:
      (
        ~ghost: Ghost.t('draggableId, 'droppableId),
        ~point: Point.t,
        ~element: Dom.htmlElement,
        ~subscriptions: Subscriptions.t
      ) =>
      unit,
    startDropping: Ghost.t('draggableId, 'droppableId) => unit,
    cancelDropping: Ghost.t('draggableId, 'droppableId) => unit,
  };
};

module Payload = {
  type t('draggableId, 'droppableId) = {
    context: Context.t('draggableId, 'droppableId),
  };
};

module DropResult = {
  type droppables('droppableId) = {
    prev: 'droppableId,
    next: 'droppableId,
  };

  type t('draggableId, 'droppableId) =
    | SameTarget('draggableId, 'droppableId, array('draggableId))
    | NewTarget('draggableId, droppables('droppableId), array('draggableId))
    | NoChanges;

  type draggableIntermediateResult('draggableId) = {
    id: 'draggableId,
    rect: Rect.t,
    margins: Margins.t,
    ghost: bool,
  };
};
