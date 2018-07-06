open Webapi.Dom;

open Dnd__Types;

module Geometry = Dnd__Geometry;

module Speed = {
  type t = {
    x: int,
    y: int,
  };

  let startFrom = 0.25;
  let maxSpeedAt = 0.05;
  let maxSpeed = 28;

  let calculate =
      (point: Point.t, dimensions: Dimensions.t, direction: Direction.t)
      : option(t) => {
    let height = dimensions.height |> float_of_int;
    switch (direction) {
    | Alpha =>
      let startFrom = height *. startFrom |> int_of_float;
      let maxSpeedAt = height *. maxSpeedAt |> int_of_float;
      if (point.y <= startFrom && point.y > maxSpeedAt) {
        let scrollRange = startFrom - maxSpeedAt |> float_of_int;
        let distanceFromStart = startFrom - point.y |> float_of_int;
        let powed =
          Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0);
        Some({
          x: 0,
          y: - ((maxSpeed |> float_of_int) *. powed |> int_of_float),
        });
      } else if (point.y <= maxSpeedAt) {
        Some({x: 0, y: - maxSpeed});
      } else {
        None;
      };

    | Omega =>
      let startFrom = height *. (1.0 -. startFrom) |> int_of_float;
      let maxSpeedAt = height *. (1.0 -. maxSpeedAt) |> int_of_float;
      if (point.y >= startFrom && point.y < maxSpeedAt) {
        let scrollRange = maxSpeedAt - startFrom |> float_of_int;
        let distanceFromStart = point.y - startFrom |> float_of_int;
        let powed =
          Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0);
        Some({x: 0, y: (maxSpeed |> float_of_int) *. powed |> int_of_float});
      } else if (point.y >= maxSpeedAt) {
        Some({x: 0, y: maxSpeed});
      } else {
        None;
      };
    };
  };
};

type onWindowScroll = unit => unit;
type onElementScroll('droppableId) =
  ('droppableId, ScrollableElement.t) => unit;

type scroller('droppableId) =
  | Window(onWindowScroll => option(Webapi.rafId))
  | Element(onElementScroll('droppableId) => option(Webapi.rafId));

let getWindowScrollDirection =
    (point: RelativityBag.t(Point.t), viewport: Dimensions.t) =>
  Direction.(
    if (viewport.height / 2 > point.viewport.y) {
      Alpha;
    } else {
      Omega;
    }
  );

let getElementScrollDirection =
    (point: Point.t, scrollable: ScrollableElement.t) =>
  Direction.(
    if (scrollable.geometry.dimensions.height / 2 > point.y) {
      Alpha;
    } else {
      Omega;
    }
  );

let canScrollWindow =
    (scroll: Scroll.t, viewport: Dimensions.t, direction: Direction.t) =>
  switch (direction) {
  | Alpha => scroll.current.y > 0
  | Omega => scroll.max.height !== scroll.current.y + viewport.height
  };

let canScrollElement =
    (scrollable: ScrollableElement.t, direction: Direction.t) =>
  switch (direction) {
  | Alpha => scrollable.scroll.current.y > 0
  | Omega =>
    scrollable.scroll.max.height !== scrollable.geometry.dimensions.height
    + scrollable.scroll.current.y
  };

let windowScroller =
    (
      point: RelativityBag.t(Point.t),
      viewport: Dimensions.t,
      direction: Direction.t,
    ) =>
  Window(
    onScroll =>
      Speed.calculate(point.viewport, viewport, direction)
      |. Option.map(speed =>
           Webapi.requestCancellableAnimationFrame(_ => {
             window |> Window.scrollBy(speed.x, speed.y);
             onScroll();
           })
         ),
  );

let elementScroller =
    (
      point: Point.t,
      scrollable: ScrollableElement.t,
      direction: Direction.t,
      droppableId,
    ) =>
  Element(
    onScroll =>
      Speed.calculate(point, scrollable.geometry.dimensions, direction)
      |. Option.map(speed =>
           Webapi.requestCancellableAnimationFrame(_ => {
             scrollable.element
             |. HtmlElement.setScrollTop(
                  scrollable.scroll.current.y + speed.y,
                );
             onScroll(droppableId, scrollable);
           })
         ),
  );

let relToScrollable =
    (point: RelativityBag.t(Point.t), scrollable: ScrollableElement.t) =>
  Point.{
    x: point.page.x - scrollable.geometry.rect.page.left,
    y: point.page.y - scrollable.geometry.rect.page.top,
  };

/*
 * Scroller can either scroll window or scrollable element:
 *   a. if ghost is inside scrollable which is bigger than viewport
 *      -> scroll window until edge of scrollable then scroll scrollable
 *   b. if ghost is inside scrollable which is smaller than viewport
 *      -> scroll scrollable then scroll window (if required)
 *   c. if ghost is not inside scrollable
 *      -> scroll window (if required)
 */
let getScroller =
    (
      ~point: RelativityBag.t(Point.t),
      ~scroll: Scroll.t,
      ~scrollable: option(('droppableId, option(ScrollableElement.t))),
      ~viewport: Dimensions.t,
    ) =>
  switch (scrollable) {
  | Some((droppableId, Some(scrollable)))
      when scrollable.geometry.dimensions.height > viewport.height =>
    let windowScrollDirection = getWindowScrollDirection(point, viewport);
    let canScrollWindow =
      canScrollWindow(scroll, viewport, windowScrollDirection);
    if (canScrollWindow) {
      Some(windowScroller(point, viewport, windowScrollDirection));
    } else {
      let point = point |. relToScrollable(scrollable);
      let elementScrollDirection =
        getElementScrollDirection(point, scrollable);
      let canScrollElement =
        canScrollElement(scrollable, elementScrollDirection);

      if (canScrollElement) {
        Some(
          elementScroller(
            point,
            scrollable,
            elementScrollDirection,
            droppableId,
          ),
        );
      } else {
        None;
      };
    };

  | Some((droppableId, Some(scrollable))) =>
    let point = point |. relToScrollable(scrollable);
    let elementScrollDirection = getElementScrollDirection(point, scrollable);
    let canScrollElement =
      canScrollElement(scrollable, elementScrollDirection);

    if (canScrollElement) {
      Some(
        elementScroller(
          point,
          scrollable,
          elementScrollDirection,
          droppableId,
        ),
      );
    } else {
      None;
    };

  | Some((_, None))
  | None =>
    let windowScrollDirection = getWindowScrollDirection(point, viewport);
    let canScrollWindow =
      canScrollWindow(scroll, viewport, windowScrollDirection);

    if (canScrollWindow) {
      Some(windowScroller(point, viewport, windowScrollDirection));
    } else {
      None;
    };
  };
