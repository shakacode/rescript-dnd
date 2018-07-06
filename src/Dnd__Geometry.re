open Webapi.Dom;
open Dnd__Types;

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
  Dimensions.{width: rect |. DomRect.width, height: rect |. DomRect.height};

let getMargins = (style: Dom.cssStyleDeclaration) =>
  Margins.{
    top: style |> CssStyleDeclaration.marginTop |> Style.stripPx,
    bottom: style |> CssStyleDeclaration.marginBottom |> Style.stripPx,
    left: style |> CssStyleDeclaration.marginLeft |> Style.stripPx,
    right: style |> CssStyleDeclaration.marginRight |> Style.stripPx,
  };

let getPaddings = (style: Dom.cssStyleDeclaration) =>
  Paddings.{
    top: style |> CssStyleDeclaration.paddingTop |> Style.stripPx,
    bottom: style |> CssStyleDeclaration.paddingBottom |> Style.stripPx,
    left: style |> CssStyleDeclaration.paddingLeft |> Style.stripPx,
    right: style |> CssStyleDeclaration.paddingRight |> Style.stripPx,
  };

let getBorders = (style: Dom.cssStyleDeclaration) =>
  Borders.{
    top: style |> CssStyleDeclaration.borderTopWidth |> Style.stripPx,
    bottom: style |> CssStyleDeclaration.borderBottomWidth |> Style.stripPx,
    left: style |> CssStyleDeclaration.borderLeftWidth |> Style.stripPx,
    right: style |> CssStyleDeclaration.borderRightWidth |> Style.stripPx,
  };

let getPageRect = (rect: DomRect.t, scroll: Point.t) =>
  Rect.{
    top: (rect |. DomRect.top) + scroll.y,
    bottom: (rect |. DomRect.bottom) + scroll.y,
    left: (rect |. DomRect.left) + scroll.x,
    right: (rect |. DomRect.right) + scroll.x,
  };

let getViewportRect = (rect: DomRect.t) =>
  Rect.{
    top: rect |. DomRect.top,
    bottom: rect |. DomRect.bottom,
    left: rect |. DomRect.left,
    right: rect |. DomRect.right,
  };

let getPageRectFromViewportRect = (viewport: Rect.t, scroll: Point.t) =>
  Rect.{
    top: viewport.top + scroll.y,
    bottom: viewport.bottom + scroll.y,
    left: viewport.left + scroll.x,
    right: viewport.right + scroll.x,
  };

let getGeometry = (rect, style, scroll) =>
  Geometry.{
    rect:
      RelativityBag.{
        page: rect |. getPageRect(scroll),
        viewport: rect |> getViewportRect,
      },
    dimensions: rect |> getDimensions,
    margins: style |> getMargins,
    borders: style |> getBorders,
    paddings: style |> getPaddings,
  };

let getElementGeometry = (element: Dom.htmlElement) =>
  getGeometry(
    element |. HtmlElement.getBoundingClientRect,
    window
    |> Window.getComputedStyle(element |> Html.castHtmlElementToElement),
  );

let getViewport = () => {
  let element =
    document
    |> Document.asHtmlDocument
    |> Option.getExn
    |> HtmlDocument.documentElement
    |> Element.asHtmlElement
    |> Option.getExn;

  Dimensions.{
    width: element |> HtmlElement.clientWidth,
    height: element |> HtmlElement.clientHeight,
  };
};

let shiftInternalSibling =
    (
      ghost: Dimensions.t,
      item: Geometry.t,
      scroll: Scroll.t,
      scrollable: option(ScrollableElement.t),
      shift: option(Direction.t),
    ) => {
  let scrollableDeltaY =
    scrollable
    |. Option.map(scrollable => scrollable.scroll.delta.y)
    |. Option.getWithDefault(0);
  let deltaY =
    switch (shift) {
    | None => scroll.delta.y + scrollableDeltaY
    | Some(Alpha) =>
      scroll.delta.y
      + scrollableDeltaY
      + ghost.height
      + item.margins.top
      + item.margins.bottom
    | Some(Omega) =>
      scroll.delta.y
      + scrollableDeltaY
      - ghost.height
      - item.margins.top
      - item.margins.bottom
    };
  let viewport =
    Rect.{
      ...item.rect.viewport,
      top: item.rect.viewport.top - deltaY,
      bottom: item.rect.viewport.bottom - deltaY,
    };
  let page = scroll.current |> getPageRectFromViewportRect(viewport);

  RelativityBag.{page, viewport};
};

let shiftExternalSibling =
    (
      ghost: Dimensions.t,
      item: Geometry.t,
      scroll: Scroll.t,
      scrollable: option(ScrollableElement.t),
      shift: option(Direction.t),
    ) => {
  let scrollableDeltaY =
    scrollable
    |. Option.map(scrollable => scrollable.scroll.delta.y)
    |. Option.getWithDefault(0);
  let deltaY =
    switch (shift) {
    | None
    | Some(Alpha) => scroll.delta.y + scrollableDeltaY
    | Some(Omega) =>
      scroll.delta.y
      + scrollableDeltaY
      - ghost.height
      - item.margins.top
      - item.margins.bottom
    };
  let viewport =
    Rect.{
      ...item.rect.viewport,
      top: item.rect.viewport.top - deltaY,
      bottom: item.rect.viewport.bottom - deltaY,
    };
  let page = scroll.current |> getPageRectFromViewportRect(viewport);

  RelativityBag.{page, viewport};
};

let isWithin = (point: Point.t, rect: Rect.t) =>
  point.x >= rect.left
  && point.x <= rect.right
  && point.y >= rect.top
  && point.y <= rect.bottom;

let isWithinWithOffset = (point: Point.t, rect: Rect.t, offset: Offset.t) =>
  point.x >= rect.left
  - offset.left
  && point.x <= rect.right
  + offset.right
  && point.y >= rect.top
  - offset.top
  && point.y <= rect.bottom
  + offset.bottom;

let isAbove = (ghost: Rect.t, item: Rect.t) => {
  let ghostHeight = ghost.bottom - ghost.top;
  let itemHeight = item.bottom - item.top;

  let ghostCenter = ghost.top + ghostHeight / 2;
  let itemCenter = item.top + itemHeight / 2;

  ghostCenter < itemCenter;
};

let isAboveAdjusted =
    (ghost: Rect.t, item: Rect.t, direction: option(Direction.t)) => {
  let ghostHeight = ghost.bottom - ghost.top;
  let itemHeight = item.bottom - item.top;

  let ghostCenter = ghost.top + ghostHeight / 2;
  let itemCenter = item.top + itemHeight / 2;

  let height = ghostHeight < itemHeight ? ghostHeight : itemHeight;
  let directionFactor = (height |> float_of_int) *. 0.55 |> int_of_float;

  switch (direction) {
  | Some(Alpha) => ghostCenter - directionFactor < itemCenter
  | Some(Omega) => ghostCenter + directionFactor < itemCenter
  | None => ghostCenter < itemCenter
  };
};
