module Point = {
  type t = {
    x: float,
    y: float,
  }
}

module Delta = {
  type t = {
    x: float,
    y: float,
  }
}

module Distance = {
  type t = {
    x: float,
    y: float,
  }
}

module Dimensions = {
  type t = {
    width: float,
    height: float,
  }
}

module Rect = {
  type t = {
    top: float,
    bottom: float,
    left: float,
    right: float,
  }
}

module Margins = {
  type t = {
    top: float,
    bottom: float,
    left: float,
    right: float,
  }
}

module Borders = {
  type t = {
    top: float,
    bottom: float,
    left: float,
    right: float,
  }
}

module Paddings = {
  type t = {
    top: float,
    bottom: float,
    left: float,
    right: float,
  }
}

module Offset = {
  type t = {
    top: float,
    bottom: float,
    left: float,
    right: float,
  }
}

module Scroll = {
  type t = {
    initial: Point.t,
    current: Point.t,
    delta: Delta.t,
    max: Distance.t,
  }
}

module CanScroll = {
  type t = {
    x: bool,
    y: bool,
  }
}

module Axis = {
  type t =
    | X
    | Y
}

module Arrow = {
  type t =
    | Up
    | Down
    | Left
    | Right
}

module Direction = {
  type t =
    | Alpha
    | Omega
}

module AxisDirection = {
  type t = {
    x: Direction.t,
    y: Direction.t,
  }
}

module RelativityBag = {
  type t<'a> = {
    page: 'a,
    viewport: 'a,
  }
}

module Geometry = {
  type t = {
    rect: RelativityBag.t<Rect.t>,
    dimensions: Dimensions.t,
    margins: Margins.t,
    borders: Borders.t,
    paddings: Paddings.t,
  }
}

module Shift = {
  type t = option<Direction.t>
}

module ScrollableElement = {
  type t = {
    element: Dom.htmlElement,
    geometry: Geometry.t,
    scroll: Scroll.t,
  }
}

module ItemBag = {
  type t<'itemId, 'containerId> = {
    id: 'itemId,
    containerId: 'containerId,
    originalIndex: int,
    targetIndex: int,
    element: Dom.htmlElement,
    shift: Shift.t,
    geometry: option<Geometry.t>,
    animating: bool,
    getGeometry: unit => Geometry.t,
  }

  type registrationPayload<'itemId, 'containerId> = {
    id: 'itemId,
    containerId: 'containerId,
    index: int,
    element: Dom.htmlElement,
    getGeometry: unit => Geometry.t,
  }
}

module ContainerBag = {
  type t<'itemId, 'containerId> = {
    id: 'containerId,
    axis: Axis.t,
    lockAxis: bool,
    element: Dom.htmlElement,
    geometry: option<Geometry.t>,
    scrollable: option<ScrollableElement.t>,
    accept: option<'itemId => bool>,
    getGeometryAndScrollable: unit => (Geometry.t, option<ScrollableElement.t>),
  }

  type registrationPayload<'itemId, 'containerId> = {
    id: 'containerId,
    axis: Axis.t,
    lockAxis: bool,
    element: Dom.htmlElement,
    accept: option<'itemId => bool>,
    getGeometryAndScrollable: unit => (Geometry.t, option<ScrollableElement.t>),
  }
}

module Ghost = {
  type t<'itemId, 'containerId> = {
    element: Dom.htmlElement,
    itemId: 'itemId,
    originalContainer: 'containerId,
    targetContainer: option<'containerId>,
    targetingOriginalContainer: bool,
    axis: Axis.t,
    lockAxis: bool,
    direction: option<Direction.t>,
    dimensions: Dimensions.t,
    margins: Margins.t,
    borders: Borders.t,
    delta: Delta.t,
    departurePoint: RelativityBag.t<Point.t>,
    currentPoint: RelativityBag.t<Point.t>,
    departureRect: RelativityBag.t<Rect.t>,
    currentRect: RelativityBag.t<Rect.t>,
  }
}

module Pawn = {
  type t<'itemId, 'containerId> = {
    element: Dom.htmlElement,
    itemId: 'itemId,
    originalContainer: 'containerId,
    targetContainer: 'containerId,
    targetingOriginalContainer: bool,
    axis: Axis.t,
    originalIndex: int,
    dimensions: Dimensions.t,
    margins: Margins.t,
    borders: Borders.t,
    delta: Delta.t,
    departurePoint: RelativityBag.t<Point.t>,
    currentPoint: RelativityBag.t<Point.t>,
    departureRect: RelativityBag.t<Rect.t>,
    currentRect: RelativityBag.t<Rect.t>,
  }
}

module Subscriptions = {
  type t = {
    install: unit => unit,
    drop: unit => unit,
  }
}

module ReorderResult = {
  type rec t<'itemId, 'containerId> =
    | SameContainer('itemId, placement<'itemId>)
    | NewContainer('itemId, 'containerId, placement<'itemId>)

  and placement<'itemId> =
    | Before('itemId)
    | Last
}

module Status = {
  type t<'itemId, 'containerId> =
    | StandBy
    | Collecting(
        'itemId,
        'containerId,
        RelativityBag.t<Point.t>,
        RelativityBag.t<Point.t>,
        [#Mouse | #Touch],
      )
    | Dragging(Ghost.t<'itemId, 'containerId>, Subscriptions.t)
    | Dropping(Ghost.t<'itemId, 'containerId>, option<ReorderResult.t<'itemId, 'containerId>>)
}
