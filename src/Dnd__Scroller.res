open Dnd__Types

module Geometry = Dnd__Geometry

module Speed = {
  type t = {
    x: option<float>,
    y: option<float>,
  }

  let startFrom = 0.25
  let maxSpeedAt = 0.05
  let maxSpeed = 28.0

  let calculateAxis = (~position: float, ~dimension: float, ~direction: Direction.t) =>
    switch direction {
    | Alpha =>
      let startFrom = dimension *. startFrom
      let maxSpeedAt = dimension *. maxSpeedAt
      if position <= startFrom && position > maxSpeedAt {
        let scrollRange = startFrom -. maxSpeedAt
        let distanceFromStart = startFrom -. position
        let powed = Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0)
        Some(-.(maxSpeed *. powed))
      } else if position <= maxSpeedAt {
        Some(-.maxSpeed)
      } else {
        None
      }

    | Omega =>
      let startFrom = dimension *. (1.0 -. startFrom)
      let maxSpeedAt = dimension *. (1.0 -. maxSpeedAt)
      if position >= startFrom && position < maxSpeedAt {
        let scrollRange = maxSpeedAt -. startFrom
        let distanceFromStart = position -. startFrom
        let powed = Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0)
        Some(maxSpeed *. powed)
      } else if position >= maxSpeedAt {
        Some(maxSpeed)
      } else {
        None
      }
    }

  let calculate = (point: Point.t, dimensions: Dimensions.t, direction: AxisDirection.t): t => {
    x: calculateAxis(~position=point.x, ~dimension=dimensions.width, ~direction=direction.x),
    y: calculateAxis(~position=point.y, ~dimension=dimensions.height, ~direction=direction.y),
  }
}

type onWindowScroll = unit => unit
type onElementScroll = ScrollableElement.t => unit

type scroller =
  | Window(onWindowScroll => option<Webapi.rafId>)
  | Element(onElementScroll => option<Webapi.rafId>)

let getWindowScrollDirection = (point: RelativityBag.t<Point.t>, viewport: Dimensions.t) => {
  open AxisDirection
  {
    x: {
      open Direction
      if viewport.width /. 2. > point.viewport.x {
        Alpha
      } else {
        Omega
      }
    },
    y: {
      open Direction
      if viewport.height /. 2. > point.viewport.y {
        Alpha
      } else {
        Omega
      }
    },
  }
}

let getElementScrollDirection = (point: Point.t, scrollable: ScrollableElement.t) => {
  open AxisDirection
  {
    x: {
      open Direction
      if scrollable.geometry.dimensions.width /. 2. > point.x {
        Alpha
      } else {
        Omega
      }
    },
    y: {
      open Direction
      if scrollable.geometry.dimensions.height /. 2. > point.y {
        Alpha
      } else {
        Omega
      }
    },
  }
}

let canScrollWindow = (scroll: Scroll.t, viewport: Dimensions.t, direction: AxisDirection.t) => {
  open CanScroll
  {
    x: switch direction.x {
    | Alpha => scroll.current.x > 0.
    | Omega => scroll.max.x != scroll.current.x +. viewport.width
    },
    y: switch direction.y {
    | Alpha => scroll.current.y > 0.
    | Omega => scroll.max.y != scroll.current.y +. viewport.height
    },
  }
}

let canScrollElement = (scrollable: ScrollableElement.t, direction: AxisDirection.t) => {
  open CanScroll
  {
    x: switch direction.x {
    | Alpha => scrollable.scroll.current.x > 0.
    | Omega =>
      scrollable.scroll.max.x != scrollable.geometry.dimensions.width +. scrollable.scroll.current.x
    },
    y: switch direction.y {
    | Alpha => scrollable.scroll.current.y > 0.
    | Omega =>
      scrollable.scroll.max.y !=
        scrollable.geometry.dimensions.height +. scrollable.scroll.current.y
    },
  }
}

let windowScroller = (
  point: RelativityBag.t<Point.t>,
  viewport: Dimensions.t,
  direction: AxisDirection.t,
) => Window(
  onScroll => {
    let speed = Speed.calculate(point.viewport, viewport, direction)

    switch speed {
    | {x: Some(x), y: Some(y)} =>
      Some(
        Webapi.requestCancellableAnimationFrame(_ => {
          {
            open Webapi.Dom
            window->Window.scrollBy(x, y)
          }

          onScroll()
        }),
      )

    | {x: Some(x), y: None} =>
      Some(
        Webapi.requestCancellableAnimationFrame(_ => {
          {
            open Webapi.Dom
            window->Window.scrollBy(x, 0.)
          }

          onScroll()
        }),
      )

    | {x: None, y: Some(y)} =>
      Some(
        Webapi.requestCancellableAnimationFrame(_ => {
          {
            open Webapi.Dom
            window->Window.scrollBy(0., y)
          }

          onScroll()
        }),
      )

    | {x: None, y: None} => None
    }
  },
)

let elementScroller = (
  point: Point.t,
  scrollable: ScrollableElement.t,
  direction: AxisDirection.t,
) => Element(
  onScroll => {
    let speed = Speed.calculate(point, scrollable.geometry.dimensions, direction)

    switch speed {
    | {x: Some(x), y: Some(y)} =>
      Some({
        open Webapi
        requestCancellableAnimationFrame(_ => {
          scrollable.element->Dom.HtmlElement.setScrollLeft(scrollable.scroll.current.x +. x)
          scrollable.element->Dom.HtmlElement.setScrollTop(scrollable.scroll.current.y +. y)
          scrollable->onScroll
        })
      })

    | {x: Some(x), y: None} =>
      Some({
        open Webapi
        requestCancellableAnimationFrame(_ => {
          scrollable.element->Dom.HtmlElement.setScrollLeft(scrollable.scroll.current.x +. x)
          scrollable->onScroll
        })
      })

    | {x: None, y: Some(y)} =>
      Some({
        open Webapi
        requestCancellableAnimationFrame(_ => {
          scrollable.element->Dom.HtmlElement.setScrollTop(scrollable.scroll.current.y +. y)
          scrollable->onScroll
        })
      })

    | {x: None, y: None} => None
    }
  },
)

let relToScrollable = (point: RelativityBag.t<Point.t>, scrollable: ScrollableElement.t) => {
  open Point
  {
    x: point.page.x -. scrollable.geometry.rect.page.left,
    y: point.page.y -. scrollable.geometry.rect.page.top,
  }
}

// Scroller can either scroll window or scrollable element:
//   a. if ghost is inside scrollable which is bigger than viewport
//      -> scroll window until edge of scrollable then scroll scrollable
//   b. if ghost is inside scrollable which is smaller than viewport
//      -> scroll scrollable then scroll window (if required)
//   c. if ghost is not inside scrollable
//      -> scroll window (if required)
let getScroller = (
  ~point: RelativityBag.t<Point.t>,
  ~viewport: Dimensions.t,
  ~scroll: Scroll.t,
  ~scrollable: option<ScrollableElement.t>,
) =>
  switch scrollable {
  | Some(scrollable) if scrollable.geometry.dimensions.height > viewport.height =>
    let windowScrollDirection = getWindowScrollDirection(point, viewport)
    let canScrollWindow = canScrollWindow(scroll, viewport, windowScrollDirection)

    switch canScrollWindow {
    | {x: true, y: _}
    | {x: _, y: true} =>
      Some(windowScroller(point, viewport, windowScrollDirection))
    | {x: false, y: false} =>
      let point = point->relToScrollable(scrollable)
      let elementScrollDirection = point->getElementScrollDirection(scrollable)
      let canScrollElement = scrollable->canScrollElement(elementScrollDirection)
      switch canScrollElement {
      | {x: true, y: _}
      | {x: _, y: true} =>
        Some(elementScroller(point, scrollable, elementScrollDirection))
      | {x: false, y: false} => None
      }
    }

  | Some(scrollable) =>
    let point = point->relToScrollable(scrollable)
    let elementScrollDirection = point->getElementScrollDirection(scrollable)
    let canScrollElement = scrollable->canScrollElement(elementScrollDirection)

    switch canScrollElement {
    | {x: true, y: _}
    | {x: _, y: true} =>
      Some(elementScroller(point, scrollable, elementScrollDirection))
    | {x: false, y: false} => None
    }

  | None =>
    let windowScrollDirection = point->getWindowScrollDirection(viewport)
    let canScrollWindow = canScrollWindow(scroll, viewport, windowScrollDirection)

    switch canScrollWindow {
    | {x: true, y: _}
    | {x: _, y: true} =>
      Some(windowScroller(point, viewport, windowScrollDirection))
    | {x: false, y: false} => None
    }
  }
