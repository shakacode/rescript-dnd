open Webapi.Dom;

external castHtmlElementToElement : Dom.htmlElement => Element.t = "%identity";

let selectionCollapsed = () =>
  window |> Window.getSelection |> Selection.isCollapsed;

let clearSelection = () =>
  window |> Window.getSelection |> Selection.removeAllRanges;
