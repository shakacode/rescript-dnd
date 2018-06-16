open Webapi.Dom;

external castHtmlElementToElement : Dom.htmlElement => Element.t = "%identity";

let clearTextSelection = () =>
  switch (document |> Document.asHtmlDocument) {
  | Some(document) =>
    document |> HtmlDocument.getSelection |> Selection.removeAllRanges
  | None => ()
  };
