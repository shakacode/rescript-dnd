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

let subscribeToKeyDown = handler =>
  Window.addKeyDownEventListenerWithOptions(handler, addOptions, window);
let unsubscribeFromKeyDown = handler =>
  Window.removeKeyDownEventListenerWithOptions(
    handler,
    removeOptions,
    window,
  );

/* Mouse */
let leftClick = event => event |. ReactEventRe.Mouse.button == 0;

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
