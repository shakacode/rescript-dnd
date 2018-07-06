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

let getPageRect = (rect: DomRect.t) => {
  let scroll = Html.getScroll();

  Rect.{
    top: (rect |. DomRect.top) + scroll.y,
    bottom: (rect |. DomRect.bottom) + scroll.y,
    left: (rect |. DomRect.left) + scroll.x,
    right: (rect |. DomRect.right) + scroll.x,
  };
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

let getGeometry = (rect, style) =>
  Geometry.{
    rect:
      RelativityBag.{
        page: rect |> getPageRect,
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

let rec getClosestScrollable = (element: Dom.htmlElement) =>
  element
  |. HtmlElement.parentElement
  |. Option.flatMap(element => {
       let style = window |> Window.getComputedStyle(element);
       style |> Html.isScrollable ?
         {
           let element = element |> HtmlElement.ofElement |> Option.getExn;
           let rect = element |. HtmlElement.getBoundingClientRect;
           Some(Scrollable.{element, geometry: getGeometry(rect, style)});
         } :
         element
         |> HtmlElement.ofElement
         |> Option.getExn
         |> getClosestScrollable;
     });

let getElementGeometryAndScrollable = (element: Dom.htmlElement) => {
  let rect = element |. HtmlElement.getBoundingClientRect;
  let style =
    window
    |> Window.getComputedStyle(element |> Html.castHtmlElementToElement);

  let geometry = getGeometry(rect, style);

  let scrollable =
    if (style |> Html.isScrollable) {
      Some(Scrollable.{element, geometry});
    } else {
      element |> getClosestScrollable;
    };

  (geometry, scrollable);
};

let shiftInternalSibling =
    (
      ghost: Dimensions.t,
      scroll: Scroll.t,
      item: Geometry.t,
      shift: option(Direction.t),
    ) =>
  switch (shift) {
  | None =>
    let viewport =
      Rect.{
        ...item.rect.viewport,
        top: item.rect.viewport.top - scroll.delta.y,
        bottom: item.rect.viewport.bottom - scroll.delta.y,
      };
    let page = scroll.current |> getPageRectFromViewportRect(viewport);

    RelativityBag.{page, viewport};

  | Some(Alpha) =>
    let viewport = {
      ...item.rect.viewport,
      top:
        item.rect.viewport.top
        - scroll.delta.y
        - ghost.height
        - item.margins.top
        - item.margins.bottom,
      bottom:
        item.rect.viewport.bottom
        - scroll.delta.y
        - ghost.height
        - item.margins.top
        - item.margins.bottom,
    };
    let page = scroll.current |> getPageRectFromViewportRect(viewport);

    {page, viewport};

  | Some(Omega) =>
    let viewport = {
      ...item.rect.viewport,
      top:
        item.rect.viewport.top
        - scroll.delta.y
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
      bottom:
        item.rect.viewport.bottom
        - scroll.delta.y
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
    };
    let page = scroll.current |> getPageRectFromViewportRect(viewport);

    {page, viewport};
  };

let shiftExternalSibling =
    (
      ghost: Dimensions.t,
      scroll: Scroll.t,
      item: Geometry.t,
      shift: option(Direction.t),
    ) =>
  switch (shift) {
  | None
  | Some(Alpha) =>
    let viewport =
      Rect.{
        ...item.rect.viewport,
        top: item.rect.viewport.top - scroll.delta.y,
        bottom: item.rect.viewport.bottom - scroll.delta.y,
      };
    let page = scroll.current |> getPageRectFromViewportRect(viewport);

    RelativityBag.{page, viewport};

  | Some(Omega) =>
    let viewport = {
      ...item.rect.viewport,
      top:
        item.rect.viewport.top
        - scroll.delta.y
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
      bottom:
        item.rect.viewport.bottom
        - scroll.delta.y
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
    };
    let page = scroll.current |> getPageRectFromViewportRect(viewport);

    {page, viewport};
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

/* TODO: Remove after webapi bump */
external castNodeToNullableNode : Dom.node => Js.nullable(Dom.node) =
  "%identity";

let pointWithinSelection = (point: RelativityBag.t(Point.t)) => {
  open! Webapi.Dom;

  window
  |. Window.getSelection
  |. Selection.anchorNode
  |. castNodeToNullableNode
  |. Js.Nullable.toOption
  |. Option.map(text => {
       let range = Range.make();
       range |> Range.selectNode(text);
       let rect = range |. Range.getBoundingClientRect |. getPageRect;
       range |> Range.detach;

       let vOffset = 10;
       let hOffset = 40;

       point.page
       |. isWithinWithOffset(
            rect,
            Offset.{
              top: vOffset,
              bottom: vOffset,
              left: hOffset,
              right: hOffset,
            },
          );
     })
  |. Option.getWithDefault(false);
};
