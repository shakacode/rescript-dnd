open Webapi.Dom;
open Dnd__Types;

module Html = Dnd__Html;
module Style = Dnd__Style;

let getDirection = (~was, ~is) => Direction.(was >= is ? Alpha : Omega);

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

let getAbsRect = (rect: DomRect.t) => {
  let scrollX = window |> Window.scrollX;
  let scrollY = window |> Window.scrollY;

  Rect.{
    top: scrollY + (rect |. DomRect.top),
    bottom: scrollY + (rect |. DomRect.bottom),
    left: scrollX + (rect |. DomRect.left),
    right: scrollX + (rect |. DomRect.right),
  };
};

let getAbsCenter = (rect: DomRect.t) => {
  let scrollX = window |> Window.scrollX;
  let scrollY = window |> Window.scrollY;

  let top = scrollY + (rect |. DomRect.top);
  let bottom = scrollY + (rect |. DomRect.bottom);
  let left = scrollX + (rect |. DomRect.left);
  let right = scrollX + (rect |. DomRect.right);

  Point.{x: left + (right - left) / 2, y: top + (bottom - top) / 2};
};

let getGeometry = (element: Dom.htmlElement) => {
  let rect = element |. HtmlElement.getBoundingClientRect;
  let style =
    window
    |> Window.getComputedStyle(element |> Html.castHtmlElementToElement);

  Geometry.{
    rect: rect |> getAbsRect,
    dimensions: rect |> getDimensions,
    margins: style |> getMargins,
    borders: style |> getBorders,
    paddings: style |> getPaddings,
  };
};

let shiftInternalSibling =
    (ghost: Dimensions.t, item: Geometry.t, shift: option(Direction.t)) =>
  switch (shift) {
  | None => item.rect
  | Some(Alpha) => {
      ...item.rect,
      top:
        item.rect.top - ghost.height - item.margins.top - item.margins.bottom,
      bottom:
        item.rect.bottom
        - ghost.height
        - item.margins.top
        - item.margins.bottom,
    }
  | Some(Omega) => {
      ...item.rect,
      top:
        item.rect.top + ghost.height + item.margins.top + item.margins.bottom,
      bottom:
        item.rect.bottom
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
    }
  };

let shiftExternalSibling =
    (ghost: Dimensions.t, item: Geometry.t, shift: option(Direction.t)) =>
  switch (shift) {
  | None
  | Some(Alpha) => item.rect
  | Some(Omega) => {
      ...item.rect,
      top:
        item.rect.top + ghost.height + item.margins.top + item.margins.bottom,
      bottom:
        item.rect.bottom
        + ghost.height
        + item.margins.top
        + item.margins.bottom,
    }
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

let isAboveAdjusted = (ghost: Rect.t, item: Rect.t, direction: Direction.t) => {
  let directionFactor = 20;

  let ghostHeight = ghost.bottom - ghost.top;
  let itemHeight = item.bottom - item.top;

  let ghostCenter = ghost.top + ghostHeight / 2;
  let itemCenter = item.top + itemHeight / 2;

  switch (direction) {
  | Alpha => ghostCenter - directionFactor < itemCenter
  | Omega => ghostCenter + directionFactor < itemCenter
  };
};

/* TODO: Remove after webapi bump */
external castNodeToNullableNode : Dom.node => Js.nullable(Dom.node) =
  "%identity";

let pointWithinSelection = point => {
  open! Webapi.Dom;

  window
  |. Window.getSelection
  |. Selection.anchorNode
  |. castNodeToNullableNode
  |. Js.Nullable.toOption
  |. Option.map(text => {
       let range = Range.make();
       range |> Range.selectNode(text);
       let rect = range |. Range.getBoundingClientRect |. getAbsRect;
       range |> Range.detach;

       let vOffset = 10;
       let hOffset = 40;

       point
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
