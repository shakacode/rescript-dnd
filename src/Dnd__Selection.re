open Dnd__Types;

module Geometry = Dnd__Geometry;
module Scrollable = Dnd__Scrollable;

/* TODO: Remove after webapi bump */
external castNodeToNullableNode : Dom.node => Js.nullable(Dom.node) =
  "%identity";

let selectionCollapsed = () =>
  Webapi.Dom.(window |> Window.getSelection |> Selection.isCollapsed);

let clearSelection = () =>
  Webapi.Dom.(window |> Window.getSelection |> Selection.removeAllRanges);

let pointWithinSelection = (point: RelativityBag.t(Point.t)) =>
  Webapi.Dom.(
    window
    |. Window.getSelection
    |. Selection.anchorNode
    |. castNodeToNullableNode
    |. Js.Nullable.toOption
    |. Option.map(text => {
         open! Webapi.Dom;

         let scroll = Scrollable.Window.getScrollPosition();
         let range = Range.make();
         range |> Range.selectNode(text);
         let rect =
           range
           |. Range.getBoundingClientRect
           |. Geometry.getPageRect(scroll);
         range |> Range.detach;

         let vOffset = 10;
         let hOffset = 40;

         point.page
         |. Geometry.isWithinWithOffset(
              rect,
              Offset.{
                top: vOffset,
                bottom: vOffset,
                left: hOffset,
                right: hOffset,
              },
            );
       })
    |. Option.getWithDefault(false)
  );
