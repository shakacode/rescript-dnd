open Dnd__Types;

module Geometry = Dnd__Geometry;

module Window = {
  open Webapi.Dom;

  let getScrollPosition = () =>
    Point.{x: window |> Window.pageXOffset, y: window |> Window.pageYOffset};

  let getMaxScroll = () => {
    let element =
      document
      |> Document.asHtmlDocument
      |> Option.getExn
      |> HtmlDocument.documentElement
      |> Element.asHtmlElement
      |> Option.getExn;

    Distance.{
      x: element |> HtmlElement.scrollWidth,
      y: element |> HtmlElement.scrollHeight,
    };
  };
};

module Element = {
  [@bs.get] external overflowX : Dom.cssStyleDeclaration => string = "";
  [@bs.get] external overflowY : Dom.cssStyleDeclaration => string = "";

  let isScrollable = style =>
    Webapi.Dom.[
      style |> CssStyleDeclaration.overflow,
      style |> overflowX,
      style |> overflowY,
    ]
    |. List.some(
         fun
         | "auto"
         | "scroll" => true
         | _ => false,
       );

  let getScrollPosition = element =>
    Point.{
      x: Webapi.Dom.(element |> HtmlElement.scrollLeft),
      y: Webapi.Dom.(element |> HtmlElement.scrollTop),
    };

  let getMaxScroll = element =>
    Distance.{
      x: Webapi.Dom.(element |> HtmlElement.scrollWidth),
      y: Webapi.Dom.(element |> HtmlElement.scrollHeight),
    };

  let rec getClosestScrollable = (element: Dom.htmlElement) =>
    element
    |. Webapi.Dom.HtmlElement.parentElement
    |. Option.flatMap(element => {
         let style =
           Webapi.Dom.window |> Webapi.Dom.Window.getComputedStyle(element);
         if (style |> isScrollable) {
           let element =
             Webapi.Dom.(element |> HtmlElement.ofElement |> Option.getExn);
           let rect =
             Webapi.Dom.(element |> HtmlElement.getBoundingClientRect);
           let maxScroll = element |> getMaxScroll;
           let windowScrollPosition = Window.getScrollPosition();
           let elementScrollPosition = element |> getScrollPosition;

           Some(
             ScrollableElement.{
               element,
               geometry:
                 Geometry.getGeometry(rect, style, windowScrollPosition),
               scroll:
                 Scroll.{
                   max: maxScroll,
                   initial: elementScrollPosition,
                   current: elementScrollPosition,
                   delta: {
                     x: 0,
                     y: 0,
                   },
                 },
             },
           );
         } else {
           element
           |> Webapi.Dom.HtmlElement.ofElement
           |> Option.getExn
           |> getClosestScrollable;
         };
       });
};
