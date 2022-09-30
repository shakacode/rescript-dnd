open Dnd__Types

module Geometry = Dnd__Geometry

module Window = {
  open Webapi.Dom

  let getScrollPosition = () => {
    open Point
    {x: window->Window.pageXOffset, y: window->Window.pageYOffset}
  }

  let getMaxScroll = () => {
    let element =
      document
      ->Document.unsafeAsHtmlDocument
      ->HtmlDocument.documentElement
      ->Element.unsafeAsHtmlElement

    open Distance
    {
      x: element->HtmlElement.scrollWidth->Float.fromInt,
      y: element->HtmlElement.scrollHeight->Float.fromInt,
    }
  }
}

module Element = {
  @get
  external overflowX: Dom.cssStyleDeclaration => string = "overflowX"
  @get
  external overflowY: Dom.cssStyleDeclaration => string = "overflowY"

  let isScrollable = style =>
    {
      open Webapi.Dom
      list{style->CssStyleDeclaration.overflow, style->overflowX, style->overflowY}
    }->List.some(x =>
      switch x {
      | "auto"
      | "scroll" => true
      | _ => false
      }
    )

  let getScrollPosition = element => {
    open Point
    {
      x: {
        open Webapi.Dom
        element->HtmlElement.scrollLeft
      },
      y: {
        open Webapi.Dom
        element->HtmlElement.scrollTop
      },
    }
  }

  let getMaxScroll = element => {
    open Distance
    {
      x: {
        open Webapi.Dom
        element->HtmlElement.scrollWidth->Float.fromInt
      },
      y: {
        open Webapi.Dom
        element->HtmlElement.scrollHeight->Float.fromInt
      },
    }
  }

  let rec getClosestScrollable = (element: Dom.htmlElement) =>
    element
    ->Webapi.Dom.HtmlElement.parentElement
    ->Option.flatMap(element => {
      let style = Webapi.Dom.window->Webapi.Dom.Window.getComputedStyle(element)
      if style->isScrollable {
        let element = {
          open Webapi.Dom
          element->HtmlElement.ofElement->Option.getExn
        }
        let rect = {
          open Webapi.Dom
          element->HtmlElement.getBoundingClientRect
        }
        let maxScroll = element->getMaxScroll
        let windowScrollPosition = Window.getScrollPosition()
        let elementScrollPosition = element->getScrollPosition

        Some({
          open ScrollableElement
          {
            element: element,
            geometry: Geometry.getGeometry(rect, style, windowScrollPosition),
            scroll: {
              open Scroll
              {
                max: maxScroll,
                initial: elementScrollPosition,
                current: elementScrollPosition,
                delta: {
                  x: 0.,
                  y: 0.,
                },
              }
            },
          }
        })
      } else {
        element->Webapi.Dom.HtmlElement.ofElement->Option.getExn->getClosestScrollable
      }
    })
}
