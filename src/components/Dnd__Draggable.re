open Dnd__React;
open Dnd__Types;
open Dnd__Units;
open Dnd__Config;

module Html = Dnd__Html;
module Events = Dnd__Events;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;
module Selection = Dnd__Selection;
module Scrollable = Dnd__Scrollable;

module Make = (Cfg: Config) => {
  type state = {
    draggableId: Cfg.Draggable.t,
    droppableId: Cfg.Droppable.t,
    context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
    element: ref(option(Dom.htmlElement)),
    moveIntervalId: ref(option(Js.Global.intervalId)),
  };

  type action =
    | RegisterDraggable;

  module Helpers = {
    let getGeometry = ({React.state}) => {
      let element = (state.element^)->Option.getExn;

      Geometry.getGeometry(
        element->Webapi.Dom.HtmlElement.getBoundingClientRect,
        element->Style.getComputedStyle,
        Scrollable.Window.getScrollPosition(),
      );
    };
  };

  module MouseInteractions = {
    let rec onMouseDown = (event, {React.state, handle}) =>
      switch (state.context.status, state.element^) {
      | (StandBy, Some(element))
          when Events.Mouse.(event->leftClick && !event->modifier) =>
        /*
         * We don't want to prevent text selection by initiating drag start
         * if user wants to select chunk of a text.
         * Sadly, there's no way to distinguish if user actually starts
         * dragging item or selecting a text. But we do can distinguish
         * if user double clicked to selected the whole word then
         * continuing to move cursor to expand the selection.
         * The difference is that in the first case selection will be collapsed
         * on the first move but in the second case there will be selected text.
         * Since `selcetionchange` event is fired after `mousedown`
         * we can't capture it right here right now but we can capture it
         * on the very first mouse move after mouse down.
         */
        let moveThreshold = 1.;
        let selectionOnStart: ref(option(bool)) = ref(None);

        let start =
          RelativityBag.{
            page:
              Point.{
                x: ReactEvent.(event->Mouse.pageX->Float.fromInt),
                y: ReactEvent.(event->Mouse.pageY->Float.fromInt),
              },
            viewport:
              Point.{
                x: ReactEvent.(event->Mouse.clientX->Float.fromInt),
                y: ReactEvent.(event->Mouse.clientY->Float.fromInt),
              },
          };

        let rec onInitialMouseMove = event => {
          let current =
            RelativityBag.{
              page:
                Point.{
                  x: Webapi.Dom.(event->MouseEvent.pageX->Float.fromInt),
                  y: Webapi.Dom.(event->MouseEvent.pageY->Float.fromInt),
                },
              viewport:
                Point.{
                  x: Webapi.Dom.(event->MouseEvent.clientX->Float.fromInt),
                  y: Webapi.Dom.(event->MouseEvent.clientY->Float.fromInt),
                },
            };

          let moved =
            Js.Math.abs_float(start.page.x -. current.page.x) > moveThreshold
            || Js.Math.abs_float(start.page.y -. current.page.y)
            > moveThreshold;

          selectionOnStart :=
            (selectionOnStart^)
            ->Option.getWithDefault(!Selection.selectionCollapsed())
            ->Some;

          let selecting =
            switch (
              selectionOnStart^,
              current->Selection.pointWithinSelection,
            ) {
            | (Some(false), _)
            | (Some(true), false) => false
            | (None, _)
            | (Some(true), true) => true
            };

          if (moved && !selecting) {
            dropInitialSubscriptions();
            Selection.clearSelection();

            let onMouseMove = onMouseMove->handle;
            let onMouseUp = onMouseUp->handle;
            let onKeyDown = onKeyDown->handle;
            let onResize = onResize->handle;
            let onVisibilityChange = onVisibilityChange->handle;

            let subscriptions =
              Subscriptions.{
                install: () => {
                  onMouseMove->Events.subscribeToMouseMove;
                  onMouseUp->Events.subscribeToMouseUp;
                  onKeyDown->Events.subscribeToKeyDown;
                  onResize->Events.subscribeToResize;
                  onVisibilityChange->Events.subscribeToVisibilityChange;
                },
                drop: () => {
                  onMouseMove->Events.unsubscribeFromMouseMove;
                  onMouseUp->Events.unsubscribeFromMouseUp;
                  onKeyDown->Events.unsubscribeFromKeyDown;
                  onResize->Events.unsubscribeFromResize;
                  onVisibilityChange->Events.unsubscribeFromVisibilityChange;
                },
              };

            state.context.startDragging(
              ~draggableId=state.draggableId,
              ~droppableId=state.droppableId,
              ~start,
              ~current,
              ~element,
              ~subscriptions,
            );
          };
        }
        and onInitialMouseUp = _ => dropInitialSubscriptions()
        and onInitialDrag = _ => dropInitialSubscriptions()
        and dropInitialSubscriptions = () => {
          Events.unsubscribeFromMouseMove(onInitialMouseMove);
          Events.unsubscribeFromMouseUp(onInitialMouseUp);
          Events.unsubscribeFromDrag(onInitialDrag);
        };

        Events.subscribeToMouseMove(onInitialMouseMove);
        Events.subscribeToMouseUp(onInitialMouseUp);
        Events.subscribeToDrag(onInitialDrag);

      | (Dragging(_, _), _)
      | (Dropping(_), _)
      | (Moving(_, _), _)
      | (CancelingMove(_), _)
      | (StandBy, _) => ()
      }
    and onMouseMove = (event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        Webapi.Dom.(event->MouseEvent.preventDefault);

        let point =
          RelativityBag.{
            page:
              Point.{
                x: Webapi.Dom.(event->MouseEvent.pageX->Float.fromInt),
                y: Webapi.Dom.(event->MouseEvent.pageY->Float.fromInt),
              },
            viewport:
              Point.{
                x: Webapi.Dom.(event->MouseEvent.clientX->Float.fromInt),
                y: Webapi.Dom.(event->MouseEvent.clientY->Float.fromInt),
              },
          };

        state.context.updateGhostPosition(point);

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onMouseUp = (_event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        Selection.clearSelection();
        state.context.drop();

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onKeyDown = (event, {React.state}) =>
      switch (state.context.status, event->Events.Keyboard.Dom.key) {
      | (Dragging(ghost, _), Esc)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        state.context.cancelDrag()

      | (Dragging(_, _), _)
      | (Dropping(_), _)
      | (Moving(_, _), _)
      | (CancelingMove(_), _)
      | (StandBy, _) => ()
      }
    and onResize = (_event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        state.context.cancelDrag()

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        state.context.cancelDrag()

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      };
  };

  module TouchInteractions = {
    let rec onTouchStart = (event, {React.state, handle}) => {
      let touch =
        event
        ->ReactEvent.Touch.touches
        ->Events.Touch.castReactTouchListToTouchArray
        ->Array.getUnsafe(0);

      switch (state.context.status, state.element^) {
      | (StandBy, Some(element)) =>
        let delay = 200;

        let start =
          RelativityBag.{
            page: Point.{x: touch##pageX, y: touch##pageY},
            viewport: Point.{x: touch##clientX, y: touch##clientY},
          };

        let timeoutId: ref(option(Js.Global.timeoutId)) = None->ref;

        let rec startDragging = () =>
          Js.Global.setTimeout(
            () => {
              dropInitialSubscriptions();
              Selection.clearSelection();

              let onTouchMove = onTouchMove->handle;
              let onTouchEnd = onTouchEnd->handle;
              let onContextMenu = onContextMenu->handle;
              let onOrientationChange = onOrientationChange->handle;
              let onVisibilityChange = onVisibilityChange->handle;

              let subscriptions =
                Subscriptions.{
                  install: () => {
                    onTouchMove->Events.subscribeToTouchMove;
                    onTouchEnd->Events.subscribeToTouchEnd;
                    onContextMenu->Events.subscribeToContextMenu;
                    onOrientationChange->Events.subscribeToOrientationChange;
                    onVisibilityChange->Events.subscribeToVisibilityChange;
                  },
                  drop: () => {
                    onTouchMove->Events.unsubscribeFromTouchMove;
                    onTouchEnd->Events.unsubscribeFromTouchEnd;
                    onContextMenu->Events.unsubscribeFromContextMenu;
                    onOrientationChange->Events.unsubscribeFromOrientationChange;
                    onVisibilityChange->Events.unsubscribeFromVisibilityChange;
                  },
                };

              state.context.startDragging(
                ~draggableId=state.draggableId,
                ~droppableId=state.droppableId,
                ~start,
                ~current=start,
                ~element,
                ~subscriptions,
              );
            },
            delay,
          )
        and cancelDrag = () =>
          switch (timeoutId^) {
          | Some(timeoutId) => timeoutId->Js.Global.clearTimeout
          | None => ()
          }
        and onInitialTouchMove = _ => cancelDrag()
        and onInitialTouchEnd = _ => cancelDrag()
        and onInitialDrag = _ => cancelDrag()
        and dropInitialSubscriptions = () => {
          onInitialTouchMove->Events.unsubscribeFromTouchMove;
          onInitialTouchEnd->Events.unsubscribeFromTouchEnd;
          onInitialDrag->Events.unsubscribeFromDrag;
        };

        onInitialTouchMove->Events.subscribeToTouchMove;
        onInitialTouchEnd->Events.subscribeToTouchEnd;
        onInitialDrag->Events.subscribeToDrag;

        timeoutId := startDragging()->Some;

      | (StandBy, _)
      | (Dragging(_, _), _)
      | (Dropping(_), _)
      | (Moving(_, _), _)
      | (CancelingMove(_), _) => ()
      };
    }
    and onTouchMove = (event, {React.state}) => {
      open Webapi.Dom;

      let event = event->Events.Touch.castEventToTouchEvent;
      let touch =
        event
        ->TouchEvent.touches
        ->Events.Touch.castDomTouchListToTouchArray
        ->Array.getUnsafe(0);

      switch (state.context.status, state.element^) {
      | (Dragging(ghost, _), Some(_))
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event->TouchEvent.preventDefault;

        let point =
          RelativityBag.{
            page: Point.{x: touch##pageX, y: touch##pageY},
            viewport: Point.{x: touch##clientX, y: touch##clientY},
          };

        state.context.updateGhostPosition(point);

      | (Dragging(_, _), _)
      | (Dropping(_), _)
      | (Moving(_, _), _)
      | (CancelingMove(_), _)
      | (StandBy, _) => ()
      };
    }
    and onTouchEnd = (event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event->Webapi.Dom.Event.preventDefault;
        state.context.drop();

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onContextMenu = (event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event->Webapi.Dom.Event.preventDefault
      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onOrientationChange = (_event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        state.context.cancelDrag()

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {React.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        state.context.cancelDrag()

      | Dragging(_, _)
      | Dropping(_)
      | Moving(_, _)
      | CancelingMove(_)
      | StandBy => ()
      };
  };

  module KeyboardInteractions = {
    let rec onKeyDown = (event, {React.state, handle}) =>
      switch (state.context.status, event->Events.Keyboard.React.key) {
      | (StandBy, Space) =>
        event->ReactEvent.Keyboard.preventDefault;

        let rec onInitialKeyUp = event =>
          switch (
            state.context.status,
            event->Events.Keyboard.Dom.key,
            state.element^,
          ) {
          | (StandBy, Space, Some(element)) =>
            dropInitialSubscriptions();

            let onKeyUp = onKeyUpWhileMoving->handle;
            let onKeyDown = onKeyDownWhileMoving->handle;

            let subscriptions =
              Subscriptions.{
                install: () => {
                  onKeyUp->Events.subscribeToKeyUp;
                  onKeyDown->Events.subscribeToKeyDown;
                },
                drop: () => {
                  onKeyUp->Events.unsubscribeFromKeyUp;
                  onKeyDown->Events.unsubscribeFromKeyDown;
                },
              };

            state.context.enterMoveMode(
              ~draggableId=state.draggableId,
              ~droppableId=state.droppableId,
              ~element,
              ~subscriptions,
            );
          | _ => dropInitialSubscriptions()
          }
        and dropInitialSubscriptions = () =>
          onInitialKeyUp->Events.unsubscribeFromKeyUp;

        onInitialKeyUp->Events.subscribeToKeyUp;
      | _ => ()
      }
    and onKeyDownWhileMoving = (event, {React.state}) => {
      let key = event->Events.Keyboard.Dom.key;

      switch (state.context.status, key) {
      | (Moving(_, _), Tab)
      | (Moving(_, _), Esc)
      | (Moving(_, _), Space) =>
        event->Webapi.Dom.KeyboardEvent.preventDefault

      | (Moving(_, _), ArrowUp)
      | (Moving(_, _), ArrowDown)
      | (Moving(_, _), ArrowLeft)
      | (Moving(_, _), ArrowRight) =>
        event->Webapi.Dom.KeyboardEvent.preventDefault;

        switch (state.moveIntervalId^) {
        | Some(intervalId) => intervalId->Js.Global.clearInterval
        | None => ()
        };

        let arrow =
          switch (key) {
          | ArrowUp => Arrow.Up
          | ArrowDown => Arrow.Down
          | ArrowLeft => Arrow.Left
          | ArrowRight => Arrow.Right
          | _ => failwith("Impossible arrow key")
          };

        state.moveIntervalId :=
          Some(
            Js.Global.setInterval(
              () => state.context.updatePawnPosition(arrow),
              Style.(animationDuration + finishDropFactor),
            ),
          );
      | _ => ()
      };
    }
    and onKeyUpWhileMoving = (event, {React.state}) => {
      let key = event->Events.Keyboard.Dom.key;

      switch (state.context.status, key) {
      | (Moving(_, _), Tab) => event->Webapi.Dom.KeyboardEvent.preventDefault

      | (Moving(_, _), Esc) =>
        event->Webapi.Dom.KeyboardEvent.preventDefault;
        state.context.cancelMove();

      | (Moving(_, _), Space)
      | (Moving(_, _), Enter) =>
        event->Webapi.Dom.KeyboardEvent.preventDefault;
        state.context.commitMove();

      | (Moving(_, _), ArrowUp)
      | (Moving(_, _), ArrowDown)
      | (Moving(_, _), ArrowLeft)
      | (Moving(_, _), ArrowRight) =>
        event->Webapi.Dom.KeyboardEvent.preventDefault;

        switch (state.moveIntervalId^) {
        | Some(intervalId) => intervalId->Js.Global.clearInterval
        | None => ()
        };

        let arrow =
          switch (key) {
          | ArrowUp => Arrow.Up
          | ArrowDown => Arrow.Down
          | ArrowLeft => Arrow.Left
          | ArrowRight => Arrow.Right
          | _ => failwith("Impossible arrow key")
          };

        state.context.updatePawnPosition(arrow);

      | _ => ()
      };
    };
  };

  type dragHandle = {
    style: ReactDom.Style.t,
    onKeyDown: ReactEvent.Keyboard.t => unit,
    onMouseDown: ReactEvent.Mouse.t => unit,
    onTouchStart: ReactEvent.Touch.t => unit,
  };

  type children =
    | Children(React.reactElement)
    | ChildrenWithDragHandle(dragHandle => React.reactElement);

  let component = React.reducerComponent("DndDraggable");
  let make =
      (
        ~id as draggableId: Cfg.Draggable.t,
        ~droppableId: Cfg.Droppable.t,
        ~index: int,
        ~context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
        ~className: option((~dragging: bool, ~moving: bool) => string)=?,
        children,
      ) => {
    ...component,
    initialState: () => {
      draggableId,
      droppableId,
      context,
      element: None->ref,
      moveIntervalId: None->ref,
    },
    didMount: ({send, handle, onUnmount}) => {
      RegisterDraggable->send;

      /* HACK: We have to add persistent event listener due to webkit bug:
       *       https://bugs.webkit.org/show_bug.cgi?id=184250
       */
      let preventTouchMoveInWebkit = (event, {React.state}) =>
        Webapi.Dom.(
          switch (state.context.status) {
          | Dragging(ghost, _)
              when
                Cfg.Draggable.eq(state.draggableId, ghost.draggableId)
                && !event->Event.defaultPrevented =>
            event->Event.preventDefault
          | _ => ()
          }
        );
      let handler = preventTouchMoveInWebkit->handle;

      handler->Events.subscribeToTouchMove;
      onUnmount(() => handler->Events.unsubscribeFromTouchMove);
    },
    willReceiveProps: ({state}) => {
      ...state,
      draggableId,
      droppableId,
      context,
    },
    didUpdate: ({oldSelf, newSelf}) =>
      switch (oldSelf.state.context.status, newSelf.state.context.status) {
      | (Dropping(_), StandBy)
      | (Moving(_, _), StandBy)
      | (CancelingMove(_), StandBy) => RegisterDraggable->(newSelf.send)
      | _ => ()
      },
    willUnmount: _ => context.disposeDraggable(draggableId),
    reducer: (action, _) =>
      switch (action) {
      | RegisterDraggable =>
        React.SideEffects(
          self =>
            context.registerDraggable({
              id: draggableId,
              droppableId,
              index,
              getGeometry: () => self->Helpers.getGeometry,
            }),
        )
      },
    render: ({state, handle}) => {
      let setElementRef = element =>
        state.element :=
          element
          ->Js.Nullable.toOption
          ->Option.map(Webapi.Dom.Element.unsafeAsHtmlElement);

      let dragHandle = {
        /* TODO: className: (~dragging, ~moving) => ..., */
        style:
          ReactDom.Style.make()
          ->ReactDom.Style.unsafeAddProp(
              "WebkitTapHighlightColor",
              "rgba(0, 0, 0, 0)",
            ),
        onKeyDown: KeyboardInteractions.onKeyDown->handle,
        onMouseDown: MouseInteractions.onMouseDown->handle,
        onTouchStart: TouchInteractions.onTouchStart->handle,
      };

      let children' =
        switch (children) {
        | Children(children) => [|children|]
        | ChildrenWithDragHandle(children) => [|dragHandle->children|]
        };

      switch (context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(draggableId, ghost.draggableId) =>
        let (width, height) =
          switch (ghost.axis) {
          | X => Style.(ghost.dimensions.width->px, 0.->px)
          | Y => Style.(0.->px, ghost.dimensions.height->px)
          };

        <>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~position="fixed",
                ~boxSizing="border-box",
                ~zIndex="10000",
                ~margin="0",
                ~overflow="visible",
                ~pointerEvents="none",
                ~userSelect="none",
                ~top=Style.(ghost.departureRect.page.top->px),
                ~left=Style.(ghost.departureRect.page.left->px),
                ~width=Style.(ghost.dimensions.width->px),
                ~height=Style.(ghost.dimensions.height->px),
                ~transform=
                  Style.translate(
                    ghost.delta.x
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.x
                      | None => Webapi.Dom.(window->Window.pageXOffset)
                      }
                    ),
                    ghost.delta.y
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.y
                      | None => Webapi.Dom.(window->Window.pageYOffset)
                      }
                    ),
                  ),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=true, ~moving=false))
            }>
            ...children'
          </div>
          <div
            style={ReactDom.Style.make(
              ~boxSizing="border-box",
              ~width,
              ~minWidth=width,
              ~height,
              ~minHeight=height,
              ~marginTop=Style.(ghost.margins.top->px),
              ~marginBottom=Style.(ghost.margins.bottom->px),
              ~marginLeft=Style.(ghost.margins.left->px),
              ~marginRight=Style.(ghost.margins.right->px),
              ~transition=Style.transition("all"),
              (),
            )}
          />
        </>;

      | Dropping(ghost) when Cfg.Draggable.eq(draggableId, ghost.draggableId) =>
        let (width, height) =
          switch (ghost.axis) {
          | X => Style.(ghost.dimensions.width->px, 0.->px)
          | Y => Style.(0.->px, ghost.dimensions.height->px)
          };

        <>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~position="fixed",
                ~boxSizing="border-box",
                ~zIndex="10000",
                ~margin="0",
                ~overflow="visible",
                ~pointerEvents="none",
                ~userSelect="none",
                ~top=Style.(ghost.departureRect.page.top->px),
                ~left=Style.(ghost.departureRect.page.left->px),
                ~width=Style.(ghost.dimensions.width->px),
                ~height=Style.(ghost.dimensions.height->px),
                ~transition=Style.transition("transform"),
                ~transform=
                  Style.translate(
                    ghost.delta.x
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.x
                      | None => Webapi.Dom.(window->Window.pageXOffset)
                      }
                    ),
                    ghost.delta.y
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.y
                      | None => Webapi.Dom.(window->Window.pageYOffset)
                      }
                    ),
                  ),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
          <div
            style={ReactDom.Style.make(
              ~boxSizing="border-box",
              ~width,
              ~minWidth=width,
              ~height,
              ~minHeight=height,
              ~marginTop=Style.(ghost.margins.top->px),
              ~marginBottom=Style.(ghost.margins.bottom->px),
              ~marginLeft=Style.(ghost.margins.left->px),
              ~marginRight=Style.(ghost.margins.right->px),
              ~transition=Style.transition("all"),
              (),
            )}
          />
        </>;

      | Moving(pawn, _) when Cfg.Draggable.eq(draggableId, pawn.draggableId) =>
        let (width, height) =
          switch (pawn.axis) {
          | X => Style.(pawn.dimensions.width->px, 0.->px)
          | Y => Style.(0.->px, pawn.dimensions.height->px)
          };

        switch (children) {
        | Children(_) =>
          <>
            <div
              ref=setElementRef
              tabIndex=0
              style={ReactDom.Style.make(
                ~position="fixed",
                ~boxSizing="border-box",
                ~zIndex="10000",
                ~margin="0",
                ~overflow="visible",
                ~transition=Style.transition("transform"),
                ~top=Style.(pawn.departureRect.page.top->px),
                ~left=Style.(pawn.departureRect.page.left->px),
                ~width=Style.(pawn.dimensions.width->px),
                ~height=Style.(pawn.dimensions.height->px),
                ~transform=
                  Style.translate(
                    pawn.delta.x
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.x
                      | None => Webapi.Dom.(window->Window.pageXOffset)
                      }
                    ),
                    pawn.delta.y
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.y
                      | None => Webapi.Dom.(window->Window.pageYOffset)
                      }
                    ),
                  ),
                (),
              )}
              className=?{
                className->Option.map(fn => fn(~dragging=false, ~moving=true))
              }
              onMouseDown={dragHandle.onMouseDown}
              onTouchStart={dragHandle.onTouchStart}>
              ...children'
            </div>
            <div
              style={ReactDom.Style.make(
                ~boxSizing="border-box",
                ~width,
                ~minWidth=width,
                ~height,
                ~minHeight=height,
                ~marginTop=Style.(pawn.margins.top->px),
                ~marginBottom=Style.(pawn.margins.bottom->px),
                ~marginLeft=Style.(pawn.margins.left->px),
                ~marginRight=Style.(pawn.margins.right->px),
                ~transition=Style.transition("all"),
                (),
              )}
            />
          </>

        | ChildrenWithDragHandle(_) =>
          <div
            ref=setElementRef
            style={ReactDom.Style.make(~boxSizing="border-box", ())}
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
        };

      | CancelingMove(pawn)
          when Cfg.Draggable.eq(draggableId, pawn.draggableId) =>
        let (width, height) =
          switch (pawn.axis) {
          | X => Style.(pawn.dimensions.width->px, 0.->px)
          | Y => Style.(0.->px, pawn.dimensions.height->px)
          };

        switch (children) {
        | Children(_) =>
          <>
            <div
              ref=setElementRef
              tabIndex=0
              style={ReactDom.Style.make(
                ~position="fixed",
                ~boxSizing="border-box",
                ~zIndex="10000",
                ~margin="0",
                ~overflow="visible",
                ~transition=Style.transition("transform"),
                ~top=Style.(pawn.departureRect.page.top->px),
                ~left=Style.(pawn.departureRect.page.left->px),
                ~width=Style.(pawn.dimensions.width->px),
                ~height=Style.(pawn.dimensions.height->px),
                ~transform=
                  Style.translate(
                    pawn.delta.x
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.x
                      | None => Webapi.Dom.(window->Window.pageXOffset)
                      }
                    ),
                    pawn.delta.y
                    -. (
                      switch (context.scroll) {
                      | Some(scroll) => scroll.current.y
                      | None => Webapi.Dom.(window->Window.pageYOffset)
                      }
                    ),
                  ),
                (),
              )}
              className=?{
                className->Option.map(fn => fn(~dragging=false, ~moving=true))
              }>
              ...children'
            </div>
            <div
              style={ReactDom.Style.make(
                ~boxSizing="border-box",
                ~width,
                ~minWidth=width,
                ~height,
                ~minHeight=height,
                ~marginTop=Style.(pawn.margins.top->px),
                ~marginBottom=Style.(pawn.margins.bottom->px),
                ~marginLeft=Style.(pawn.margins.left->px),
                ~marginRight=Style.(pawn.margins.right->px),
                ~transition=Style.transition("all"),
                (),
              )}
            />
          </>

        | ChildrenWithDragHandle(_) =>
          <div
            ref=setElementRef
            style={ReactDom.Style.make(~boxSizing="border-box", ())}
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
        };

      | Dragging(ghost, _)
      | Dropping(ghost) =>
        switch (draggableId->(context.getDraggableShift)) {
        | Some(Alpha) when ghost.targetingOriginalDroppable =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (ghost.axis) {
                  | X =>
                    Style.translate(
                      -. (
                        ghost.dimensions.width
                        +. ghost.margins.left
                        +. ghost.margins.right
                      ),
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      -. (
                        ghost.dimensions.height
                        +. ghost.margins.top
                        +. ghost.margins.bottom
                      ),
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Omega) when ghost.targetingOriginalDroppable =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (ghost.axis) {
                  | X =>
                    Style.translate(
                      ghost.dimensions.width
                      +. ghost.margins.left
                      +. ghost.margins.right,
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      ghost.dimensions.height
                      +. ghost.margins.top
                      +. ghost.margins.bottom,
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Alpha) =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Omega) =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (ghost.axis) {
                  | X =>
                    Style.translate(
                      ghost.dimensions.width
                      +. ghost.margins.left
                      +. ghost.margins.right,
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      ghost.dimensions.height
                      +. ghost.margins.top
                      +. ghost.margins.bottom,
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | None =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
        }

      | Moving(pawn, _)
      | CancelingMove(pawn) =>
        switch (draggableId->(context.getDraggableShift)) {
        | Some(Alpha) when pawn.targetingOriginalDroppable =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (pawn.axis) {
                  | X =>
                    Style.translate(
                      -. (
                        pawn.dimensions.width
                        +. pawn.margins.left
                        +. pawn.margins.right
                      ),
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      -. (
                        pawn.dimensions.height
                        +. pawn.margins.top
                        +. pawn.margins.bottom
                      ),
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Omega) when pawn.targetingOriginalDroppable =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (pawn.axis) {
                  | X =>
                    Style.translate(
                      pawn.dimensions.width
                      +. pawn.margins.left
                      +. pawn.margins.right,
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      pawn.dimensions.height
                      +. pawn.margins.top
                      +. pawn.margins.bottom,
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Alpha) =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | Some(Omega) =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                ~transform=
                  switch (pawn.axis) {
                  | X =>
                    Style.translate(
                      pawn.dimensions.width
                      +. pawn.margins.left
                      +. pawn.margins.right,
                      0.,
                    )
                  | Y =>
                    Style.translate(
                      0.,
                      pawn.dimensions.height
                      +. pawn.margins.top
                      +. pawn.margins.bottom,
                    )
                  },
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>

        | None =>
          <div
            ref=setElementRef
            style={
              ReactDom.Style.make(
                ~boxSizing="border-box",
                ~pointerEvents="none",
                ~userSelect="none",
                ~transition=Style.transition("transform"),
                (),
              )
              ->ReactDom.Style.unsafeAddProp("WebkitUserSelect", "none")
            }
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
        }

      | StandBy =>
        switch (children) {
        | Children(_) =>
          <div
            ref=setElementRef
            tabIndex=0
            style={ReactDom.Style.make(~boxSizing="border-box", ())}
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }
            onKeyDown={dragHandle.onKeyDown}
            onMouseDown={dragHandle.onMouseDown}
            onTouchStart={dragHandle.onTouchStart}>
            ...children'
          </div>
        | ChildrenWithDragHandle(_) =>
          <div
            ref=setElementRef
            style={ReactDom.Style.make(~boxSizing="border-box", ())}
            className=?{
              className->Option.map(fn => fn(~dragging=false, ~moving=false))
            }>
            ...children'
          </div>
        }
      };
    },
  };
};
