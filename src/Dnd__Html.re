open Webapi.Dom;

open Dnd__Types;

external castHtmlElementToElement : Dom.htmlElement => Element.t = "%identity";

[@bs.get] external overflowX : Dom.cssStyleDeclaration => string = "";
[@bs.get] external overflowY : Dom.cssStyleDeclaration => string = "";

let selectionCollapsed = () =>
  window |> Window.getSelection |> Selection.isCollapsed;

let clearSelection = () =>
  window |> Window.getSelection |> Selection.removeAllRanges;

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

let getScroll = () =>
  Point.{x: window |> Window.pageXOffset, y: window |> Window.pageYOffset};

let getMaxScroll = () => {
  let element =
    document
    |> Document.asHtmlDocument
    |> Option.getExn
    |> HtmlDocument.documentElement
    |> Element.asHtmlElement
    |> Option.getExn;

  Dimensions.{
    width: element |> HtmlElement.scrollWidth,
    height: element |> HtmlElement.scrollHeight,
  };
};

let isScrollable = style =>
  [
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
