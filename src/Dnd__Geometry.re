open Webapi.Dom;
open Dnd__Types;
open Dnd__Units;

module Html = Dnd__Html;
module Style = Dnd__Style;

let getDirection = (~was, ~is) =>
  if (was > is) {
    Some(Direction.Alpha);
  } else if (was < is) {
    Some(Direction.Omega);
  } else {
    None;
  };

let getDimensions = (rect: DomRect.t) =>
  Dimensions.{
    width: rect->DomRect.width->Float.fromInt,
    height: rect->DomRect.height->Float.fromInt,
  };

let getMargins = (style: Dom.cssStyleDeclaration) =>
  Margins.{
    top: style->CssStyleDeclaration.marginTop->Style.stripPx,
    bottom: style->CssStyleDeclaration.marginBottom->Style.stripPx,
    left: style->CssStyleDeclaration.marginLeft->Style.stripPx,
    right: style->CssStyleDeclaration.marginRight->Style.stripPx,
  };

let getPaddings = (style: Dom.cssStyleDeclaration) =>
  Paddings.{
    top: style->CssStyleDeclaration.paddingTop->Style.stripPx,
    bottom: style->CssStyleDeclaration.paddingBottom->Style.stripPx,
    left: style->CssStyleDeclaration.paddingLeft->Style.stripPx,
    right: style->CssStyleDeclaration.paddingRight->Style.stripPx,
  };

let getBorders = (style: Dom.cssStyleDeclaration) =>
  Borders.{
    top: style->CssStyleDeclaration.borderTopWidth->Style.stripPx,
    bottom: style->CssStyleDeclaration.borderBottomWidth->Style.stripPx,
    left: style->CssStyleDeclaration.borderLeftWidth->Style.stripPx,
    right: style->CssStyleDeclaration.borderRightWidth->Style.stripPx,
  };

let getPageRect = (rect: DomRect.t, scroll: Point.t) =>
  Rect.{
    top: rect->DomRect.top->Float.fromInt +. scroll.y,
    bottom: rect->DomRect.bottom->Float.fromInt +. scroll.y,
    left: rect->DomRect.left->Float.fromInt +. scroll.x,
    right: rect->DomRect.right->Float.fromInt +. scroll.x,
  };

let getViewportRect = (rect: DomRect.t) =>
  Rect.{
    top: rect->DomRect.top->Float.fromInt,
    bottom: rect->DomRect.bottom->Float.fromInt,
    left: rect->DomRect.left->Float.fromInt,
    right: rect->DomRect.right->Float.fromInt,
  };

let getPageRectFromViewportRect = (viewport: Rect.t, scroll: Point.t) =>
  Rect.{
    top: viewport.top +. scroll.y,
    bottom: viewport.bottom +. scroll.y,
    left: viewport.left +. scroll.x,
    right: viewport.right +. scroll.x,
  };

let getGeometry = (rect, style, scroll) =>
  Geometry.{
    rect:
      RelativityBag.{
        page: rect->getPageRect(scroll),
        viewport: rect->getViewportRect,
      },
    dimensions: rect->getDimensions,
    margins: style->getMargins,
    borders: style->getBorders,
    paddings: style->getPaddings,
  };

let getElementGeometry = (el: Dom.htmlElement) =>
  getGeometry(
    el->HtmlElement.getBoundingClientRect,
    window->Window.getComputedStyle(el->Html.castHtmlElementToElement, _),
  );

let getViewport = () => {
  let element =
    document
    ->Document.asHtmlDocument
    ->Option.getExn
    ->HtmlDocument.documentElement
    ->Element.asHtmlElement
    ->Option.getExn;

  Dimensions.{
    width: element->HtmlElement.clientWidth->Float.fromInt,
    height: element->HtmlElement.clientHeight->Float.fromInt,
  };
};

let getElementCenterRelToViewport = (rect: DomRect.t) => {
  let top = rect->DomRect.top->Float.fromInt;
  let bottom = rect->DomRect.bottom->Float.fromInt;
  let left = rect->DomRect.left->Float.fromInt;
  let right = rect->DomRect.right->Float.fromInt;

  Point.{x: left +. (right -. left) /. 2., y: top +. (bottom -. top) /. 2.};
};

let getElementCenterRelToPage = (rect: DomRect.t, scroll: Point.t) => {
  let top = scroll.y +. rect->DomRect.top->Float.fromInt;
  let bottom = scroll.y +. rect->DomRect.bottom->Float.fromInt;
  let left = scroll.x +. rect->DomRect.left->Float.fromInt;
  let right = scroll.x +. rect->DomRect.right->Float.fromInt;

  Point.{x: left +. (right -. left) /. 2., y: top +. (bottom -. top) /. 2.};
};

let shiftRects = (rect: RelativityBag.t(Rect.t), delta: Delta.t) =>
  RelativityBag.{
    page:
      Rect.{
        top: rect.page.top -. delta.y,
        bottom: rect.page.bottom -. delta.y,
        left: rect.page.left -. delta.x,
        right: rect.page.right -. delta.x,
      },
    viewport:
      Rect.{
        top: rect.viewport.top -. delta.y,
        bottom: rect.viewport.bottom -. delta.y,
        left: rect.viewport.left -. delta.x,
        right: rect.viewport.right -. delta.x,
      },
  };

let shiftViewportRect = (rect: RelativityBag.t(Rect.t), delta: Delta.t) =>
  RelativityBag.{
    ...rect,
    viewport:
      Rect.{
        top: rect.viewport.top -. delta.y,
        bottom: rect.viewport.bottom -. delta.y,
        left: rect.viewport.left -. delta.x,
        right: rect.viewport.right -. delta.x,
      },
  };

let shiftInternalSibling =
    (
      axis: Axis.t,
      ghost: Dimensions.t,
      item: Geometry.t,
      scroll: Scroll.t,
      scrollable: option(ScrollableElement.t),
      shift: option(Direction.t),
    ) =>
  switch (axis) {
  | X =>
    let scrollableDeltaX =
      scrollable
      ->Option.map(scrollable => scrollable.scroll.delta.x)
      ->Option.getWithDefault(0.);
    let deltaX =
      switch (shift) {
      | None => scroll.delta.x +. scrollableDeltaX
      | Some(Alpha) =>
        scroll.delta.x
        +. scrollableDeltaX
        +. ghost.width
        +. item.margins.left
        +. item.margins.right
      | Some(Omega) =>
        scroll.delta.x
        +. scrollableDeltaX
        -. ghost.width
        -. item.margins.left
        -. item.margins.right
      };
    let viewport =
      Rect.{
        ...item.rect.viewport,
        left: item.rect.viewport.left -. deltaX,
        right: item.rect.viewport.right -. deltaX,
      };
    let page = viewport->getPageRectFromViewportRect(scroll.current);

    RelativityBag.{page, viewport};

  | Y =>
    let scrollableDeltaY =
      scrollable
      ->Option.map(scrollable => scrollable.scroll.delta.y)
      ->Option.getWithDefault(0.);
    let deltaY =
      switch (shift) {
      | None => scroll.delta.y +. scrollableDeltaY
      | Some(Alpha) =>
        scroll.delta.y
        +. scrollableDeltaY
        +. ghost.height
        +. item.margins.top
        +. item.margins.bottom
      | Some(Omega) =>
        scroll.delta.y
        +. scrollableDeltaY
        -. ghost.height
        -. item.margins.top
        -. item.margins.bottom
      };
    let viewport =
      Rect.{
        ...item.rect.viewport,
        top: item.rect.viewport.top -. deltaY,
        bottom: item.rect.viewport.bottom -. deltaY,
      };
    let page = viewport->getPageRectFromViewportRect(scroll.current);

    RelativityBag.{page, viewport};
  };

let shiftExternalSibling =
    (
      axis: Axis.t,
      ghost: Dimensions.t,
      item: Geometry.t,
      scroll: Scroll.t,
      scrollable: option(ScrollableElement.t),
      shift: option(Direction.t),
    ) =>
  switch (axis) {
  | X =>
    let scrollableDeltaX =
      scrollable
      ->Option.map(scrollable => scrollable.scroll.delta.x)
      ->Option.getWithDefault(0.);
    let deltaX =
      switch (shift) {
      | None
      | Some(Alpha) => scroll.delta.x +. scrollableDeltaX
      | Some(Omega) =>
        scroll.delta.x
        +. scrollableDeltaX
        -. ghost.width
        -. item.margins.left
        -. item.margins.right
      };
    let viewport =
      Rect.{
        ...item.rect.viewport,
        left: item.rect.viewport.left -. deltaX,
        right: item.rect.viewport.right -. deltaX,
      };
    let page = viewport->getPageRectFromViewportRect(scroll.current);

    RelativityBag.{page, viewport};

  | Y =>
    let scrollableDeltaY =
      scrollable
      ->Option.map(scrollable => scrollable.scroll.delta.y)
      ->Option.getWithDefault(0.);
    let deltaY =
      switch (shift) {
      | None
      | Some(Alpha) => scroll.delta.y +. scrollableDeltaY
      | Some(Omega) =>
        scroll.delta.y
        +. scrollableDeltaY
        -. ghost.height
        -. item.margins.top
        -. item.margins.bottom
      };
    let viewport =
      Rect.{
        ...item.rect.viewport,
        top: item.rect.viewport.top -. deltaY,
        bottom: item.rect.viewport.bottom -. deltaY,
      };
    let page = viewport->getPageRectFromViewportRect(scroll.current);

    RelativityBag.{page, viewport};
  };

let isWithin = (point: Point.t, rect: Rect.t) =>
  point.x >= rect.left
  && point.x <= rect.right
  && point.y >= rect.top
  && point.y <= rect.bottom;

let isWithinWithOffset = (point: Point.t, rect: Rect.t, offset: Offset.t) =>
  point.x >= rect.left
  -. offset.left
  && point.x <= rect.right
  +. offset.right
  && point.y >= rect.top
  -. offset.top
  && point.y <= rect.bottom
  +. offset.bottom;

let contains = (~parent: Rect.t, ~child: Rect.t) =>
  child.top > parent.top
  && child.bottom < parent.bottom
  && child.left > parent.left
  && child.right < parent.right;

let isAfore = (~subject: Rect.t, ~comparand: Rect.t, ~axis: Axis.t) =>
  switch (axis) {
  | X =>
    let subjectWidth = subject.right -. subject.left;
    let comparandWidth = comparand.right -. comparand.left;

    let subjectCenter = subject.left +. subjectWidth /. 2.;
    let comparandCenter = comparand.left +. comparandWidth /. 2.;

    subjectCenter < comparandCenter;

  | Y =>
    let subjectHeight = subject.bottom -. subject.top;
    let comparandHeight = comparand.bottom -. comparand.top;

    let subjectCenter = subject.top +. subjectHeight /. 2.;
    let comparandCenter = comparand.top +. comparandHeight /. 2.;

    subjectCenter < comparandCenter;
  };

let isAforeAdjusted =
    (
      ~subject: Rect.t,
      ~comparand: Rect.t,
      ~axis: Axis.t,
      ~direction: option(Direction.t),
    ) =>
  switch (axis) {
  | X =>
    let subjectWidth = subject.right -. subject.left;
    let comparandWidth = comparand.right -. comparand.left;

    let subjectCenter = subject.left +. subjectWidth /. 2.;
    let comparandCenter = comparand.left +. comparandWidth /. 2.;

    let width = subjectWidth < comparandWidth ? subjectWidth : comparandWidth;
    let directionFactor = width *. 0.55;

    switch (direction) {
    | Some(Alpha) => subjectCenter -. directionFactor < comparandCenter
    | Some(Omega) => subjectCenter +. directionFactor < comparandCenter
    | None => subjectCenter < comparandCenter
    };

  | Y =>
    let subjectHeight = subject.bottom -. subject.top;
    let comparandHeight = comparand.bottom -. comparand.top;

    let subjectCenter = subject.top +. subjectHeight /. 2.;
    let comparandCenter = comparand.top +. comparandHeight /. 2.;

    let height =
      subjectHeight < comparandHeight ? subjectHeight : comparandHeight;
    let directionFactor = height *. 0.55;

    switch (direction) {
    | Some(Alpha) => subjectCenter -. directionFactor < comparandCenter
    | Some(Omega) => subjectCenter +. directionFactor < comparandCenter
    | None => subjectCenter < comparandCenter
    };
  };
