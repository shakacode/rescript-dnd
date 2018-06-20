open Webapi.Dom;

/* Subscriptions */
let addOptions = {"passive": false, "once": false, "capture": false};
let removeOptions = {"passive": false, "capture": false};

let subscribeToMouseMove = handler =>
  Window.addMouseMoveEventListenerWithOptions(handler, addOptions, window);
let unsubscribeFromMouseMove = handler =>
  Window.removeMouseMoveEventListenerWithOptions(
    handler,
    removeOptions,
    window,
  );

let subscribeToMouseUp = handler =>
  Window.addMouseUpEventListenerWithOptions(handler, addOptions, window);
let unsubscribeFromMouseUp = handler =>
  Window.removeMouseUpEventListenerWithOptions(
    handler,
    removeOptions,
    window,
  );

let subscribeToDrag = handler =>
  Window.addEventListenerWithOptions("drag", handler, addOptions, window);
let unsubscribeFromDrag = handler =>
  Window.removeEventListenerWithOptions(
    "drag",
    handler,
    removeOptions,
    window,
  );

let subscribeToResize = handler =>
  Window.addEventListenerWithOptions("resize", handler, addOptions, window);
let unsubscribeFromResize = handler =>
  Window.removeEventListenerWithOptions(
    "resize",
    handler,
    removeOptions,
    window,
  );

let subscribeToKeyDown = handler =>
  Window.addKeyDownEventListenerWithOptions(handler, addOptions, window);
let unsubscribeFromKeyDown = handler =>
  Window.removeKeyDownEventListenerWithOptions(
    handler,
    removeOptions,
    window,
  );

let subscribeToTouchMove = handler =>
  Window.addEventListenerWithOptions(
    "touchmove",
    handler,
    addOptions,
    window,
  );
let unsubscribeFromTouchMove = handler =>
  Window.removeEventListenerWithOptions(
    "touchmove",
    handler,
    removeOptions,
    window,
  );

let subscribeToTouchEnd = handler =>
  Window.addEventListenerWithOptions("touchend", handler, addOptions, window);
let unsubscribeFromTouchEnd = handler =>
  Window.removeEventListenerWithOptions(
    "touchend",
    handler,
    removeOptions,
    window,
  );

let subscribeToOrientationChange = handler =>
  Window.addEventListenerWithOptions(
    "orientationchange",
    handler,
    addOptions,
    window,
  );
let unsubscribeFromOrientationChange = handler =>
  Window.removeEventListenerWithOptions(
    "orientationchange",
    handler,
    removeOptions,
    window,
  );

let subscribeToContextMenu = handler =>
  Window.addEventListenerWithOptions(
    "contextmenu",
    handler,
    addOptions,
    window,
  );
let unsubscribeFromContextMenu = handler =>
  Window.removeEventListenerWithOptions(
    "contextmenu",
    handler,
    removeOptions,
    window,
  );

let subscribeToVisibilityChange = handler =>
  Window.addEventListenerWithOptions(
    "visibilitychange",
    handler,
    addOptions,
    window,
  );
let unsubscribeFromVisibilityChange = handler =>
  Window.removeEventListenerWithOptions(
    "visibilitychange",
    handler,
    removeOptions,
    window,
  );

/* Mouse */
let leftClick = event => event |. ReactEventRe.Mouse.button == 0;
let modifier = event =>
  ReactEventRe.Mouse.(
    altKey(event) || ctrlKey(event) || metaKey(event) || shiftKey(event)
  );

/* Keyboard */
let isDomKey = (key, event) => event |> KeyboardEvent.key === key;
let isReactKey = (key, event) => event |> ReactEventRe.Keyboard.key === key;

let escKey = "Escape";
let isDomEscKey = event => event |> isDomKey(escKey);
let isReactEscKey = event => event |> isReactKey(escKey);
let onDomEscKey = (handler, event) =>
  if (event |> isDomEscKey) {
    handler();
  };
let onReactEscKey = (handler, event) =>
  if (event |> isReactEscKey) {
    handler();
  };

/* Touch */
module Touch = {
  type t = {
    .
    "identifier": string,
    "clientX": int,
    "clientY": int,
    "screenX": int,
    "screenY": int,
    "pageX": int,
    "pageY": int,
    "target": Dom.element,
  };
};

external castDomTouchListToTouchArray : TouchEvent.touchList => array(Touch.t) =
  "%identity";
external castReactTouchListToTouchArray : Js.t({..}) => array(Touch.t) =
  "%identity";
external castEventToTouchEvent : Dom.event => Dom.touchEvent = "%identity";
