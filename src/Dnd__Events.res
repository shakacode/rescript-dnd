open Webapi.Dom

/* Subscriptions */
let addOptions = {"passive": false, "once": false, "capture": false}
let removeOptions = {"passive": false, "capture": false}

let subscribeToMouseMove = handler =>
  Window.addMouseMoveEventListenerWithOptions(window, handler, addOptions)
let unsubscribeFromMouseMove = handler =>
  Window.removeMouseMoveEventListenerWithOptions(window, handler, removeOptions)

let subscribeToMouseUp = handler =>
  Window.addMouseUpEventListenerWithOptions(window, handler, addOptions)
let unsubscribeFromMouseUp = handler =>
  Window.removeMouseUpEventListenerWithOptions(window, handler, removeOptions)

let subscribeToDrag = handler =>
  Window.addEventListenerWithOptions(window, "drag", handler, addOptions)
let unsubscribeFromDrag = handler =>
  Window.removeEventListenerWithOptions(window, "drag", handler, removeOptions)

let subscribeToResize = handler =>
  Window.addEventListenerWithOptions(window, "resize", handler, addOptions)
let unsubscribeFromResize = handler =>
  Window.removeEventListenerWithOptions(window, "resize", handler, removeOptions)

let subscribeToKeyUp = handler =>
  Window.addKeyUpEventListenerWithOptions(window, handler, addOptions)
let unsubscribeFromKeyUp = handler =>
  Window.removeKeyUpEventListenerWithOptions(window, handler, removeOptions)

let subscribeToKeyDown = handler =>
  Window.addKeyDownEventListenerWithOptions(window, handler, addOptions)
let unsubscribeFromKeyDown = handler =>
  Window.removeKeyDownEventListenerWithOptions(window, handler, removeOptions)

let subscribeToTouchMove = handler =>
  Window.addEventListenerWithOptions(window, "touchmove", handler, addOptions)
let unsubscribeFromTouchMove = handler =>
  Window.removeEventListenerWithOptions(window, "touchmove", handler, removeOptions)

let subscribeToTouchEnd = handler =>
  Window.addEventListenerWithOptions(window, "touchend", handler, addOptions)
let unsubscribeFromTouchEnd = handler =>
  Window.removeEventListenerWithOptions(window, "touchend", handler, removeOptions)

let subscribeToOrientationChange = handler =>
  Window.addEventListenerWithOptions(window, "orientationchange", handler, addOptions)
let unsubscribeFromOrientationChange = handler =>
  Window.removeEventListenerWithOptions(window, "orientationchange", handler, removeOptions)

let subscribeToContextMenu = handler =>
  Window.addEventListenerWithOptions(window, "contextmenu", handler, addOptions)
let unsubscribeFromContextMenu = handler =>
  Window.removeEventListenerWithOptions(window, "contextmenu", handler, removeOptions)

let subscribeToVisibilityChange = handler =>
  Window.addEventListenerWithOptions(window, "visibilitychange", handler, addOptions)
let unsubscribeFromVisibilityChange = handler =>
  Window.removeEventListenerWithOptions(window, "visibilitychange", handler, removeOptions)

module Mouse = {
  let leftClick = event => event->ReactEvent.Mouse.button === 0

  let modifier = event => {
    open ReactEvent.Mouse
    event->altKey || (event->ctrlKey || (event->metaKey || event->shiftKey))
  }
}

module Keyboard = {
  module Key = {
    type t =
      | Esc
      | Tab
      | Space
      | Enter
      | ArrowUp
      | ArrowDown
      | ArrowLeft
      | ArrowRight
      | Other

    let fromString = x =>
      switch x {
      | "Escape" => Esc
      | "Tab" => Tab
      | " " => Space
      | "Enter" => Enter
      | "ArrowUp" => ArrowUp
      | "ArrowDown" => ArrowDown
      | "ArrowLeft" => ArrowLeft
      | "ArrowRight" => ArrowRight
      | _ => Other
      }
  }

  module Dom = {
    let key = event => event->KeyboardEvent.key->Key.fromString
  }

  module React = {
    let key = event => event->ReactEvent.Keyboard.key->Key.fromString
  }
}

module Touch = {
  module Touch = {
    type t = {
      "identifier": string,
      "clientX": float,
      "clientY": float,
      "screenX": float,
      "screenY": float,
      "pageX": float,
      "pageY": float,
      "target": Dom.element,
    }
  }

  external castDomTouchListToTouchArray: TouchEvent.touchList => array<Touch.t> = "%identity"
  external castReactTouchListToTouchArray: {..} => array<Touch.t> = "%identity"
  external castEventToTouchEvent: Dom.event => Dom.touchEvent = "%identity"
}
