open Dnd__React;
open Dnd__Config;
open Dnd__Types;

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
  };

  type action =
    | RegisterDraggable;

  module Handlers = {
    let getGeometry = ({ReasonReact.state}) => {
      let element = state.element^ |> Option.getExn;

      Geometry.getGeometry(
        element |> Webapi.Dom.HtmlElement.getBoundingClientRect,
        element |> Style.getComputedStyle,
        Scrollable.Window.getScrollPosition(),
      );
    };

    let rec onMouseDown = (event, {ReasonReact.state, handle}) =>
      switch (state.context.status, state.element^) {
      | (StandBy, Some(element))
          when Events.Mouse.(event |. leftClick && ! (event |. modifier)) =>
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
        let moveThreshold = 1;
        let selectionOnStart: ref(option(bool)) = ref(None);

        let start =
          RelativityBag.{
            page:
              Point.{
                x: ReactEventRe.(event |. Mouse.pageX),
                y: ReactEventRe.(event |. Mouse.pageY),
              },
            viewport:
              Point.{
                x: ReactEventRe.(event |. Mouse.clientX),
                y: ReactEventRe.(event |. Mouse.clientY),
              },
          };

        let rec onInitialMouseMove = event => {
          let current =
            RelativityBag.{
              page:
                Point.{
                  x: Webapi.Dom.(event |. MouseEvent.pageX),
                  y: Webapi.Dom.(event |. MouseEvent.pageY),
                },
              viewport:
                Point.{
                  x: Webapi.Dom.(event |. MouseEvent.clientX),
                  y: Webapi.Dom.(event |. MouseEvent.clientY),
                },
            };

          let moved =
            Js.Math.abs_int(start.page.x - current.page.x) > moveThreshold
            || Js.Math.abs_int(start.page.y - current.page.y) > moveThreshold;

          selectionOnStart :=
            selectionOnStart^
            |. Option.getWithDefault(! Selection.selectionCollapsed())
            |. Some;

          let selecting =
            switch (
              selectionOnStart^,
              current |. Selection.pointWithinSelection,
            ) {
            | (Some(false), _)
            | (Some(true), false) => false
            | (None, _)
            | (Some(true), true) => true
            };

          if (moved && ! selecting) {
            dropInitialSubscriptions();
            Selection.clearSelection();

            let onMouseMove = onMouseMove |. handle;
            let onMouseUp = onMouseUp |. handle;
            let onKeyDown = onKeyDown |. handle;
            let onResize = onResize |. handle;
            let onVisibilityChange = onVisibilityChange |. handle;

            let subscriptions =
              Subscriptions.{
                install: () => {
                  Events.subscribeToMouseMove(onMouseMove);
                  Events.subscribeToMouseUp(onMouseUp);
                  Events.subscribeToKeyDown(onKeyDown);
                  Events.subscribeToResize(onResize);
                  Events.subscribeToVisibilityChange(onVisibilityChange);
                },
                drop: () => {
                  Events.unsubscribeFromMouseMove(onMouseMove);
                  Events.unsubscribeFromMouseUp(onMouseUp);
                  Events.unsubscribeFromKeyDown(onKeyDown);
                  Events.unsubscribeFromResize(onResize);
                  Events.unsubscribeFromVisibilityChange(onVisibilityChange);
                },
              };

            state.context.initDrag(
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

      | (StandBy, Some(_))
      | (StandBy, None)
      | (Dragging(_, _), _)
      | (Dropping(_), _) => ()
      }
    and onMouseMove = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        Webapi.Dom.(event |. MouseEvent.preventDefault);

        let point =
          RelativityBag.{
            page:
              Point.{
                x: Webapi.Dom.(event |. MouseEvent.pageX),
                y: Webapi.Dom.(event |. MouseEvent.pageY),
              },
            viewport:
              Point.{
                x: Webapi.Dom.(event |. MouseEvent.clientX),
                y: Webapi.Dom.(event |. MouseEvent.clientY),
              },
          };

        state.context.updateGhostPosition(point);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onMouseUp = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        Selection.clearSelection();
        subscriptions.drop();
        state.context.drop();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onKeyDown = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(_, subscriptions) when event |> Events.Keyboard.isDomEscKey =>
        subscriptions.drop();
        state.context.cancelDrag();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onResize = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(_, subscriptions) =>
        subscriptions.drop();
        state.context.cancelDrag();

      | Dropping(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        subscriptions.drop();
        state.context.cancelDrag();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      };

    let rec onTouchStart = (event, {ReasonReact.state, handle}) => {
      let touch =
        event
        |. ReactEventRe.Touch.touches
        |. Events.Touch.castReactTouchListToTouchArray
        |. Array.get(0);

      switch (state.context.status, state.element^, touch) {
      | (StandBy, Some(element), Some(touch)) =>
        let delay = 200;

        let start =
          RelativityBag.{
            page: Point.{x: touch##pageX, y: touch##pageY},
            viewport: Point.{x: touch##clientX, y: touch##clientY},
          };

        let timeoutId: ref(option(Js.Global.timeoutId)) = ref(None);

        let rec startDragging = () =>
          Js.Global.setTimeout(
            () => {
              dropInitialSubscriptions();
              Selection.clearSelection();

              let onTouchMove = onTouchMove |. handle;
              let onTouchEnd = onTouchEnd |. handle;
              let onContextMenu = onContextMenu |. handle;
              let onOrientationChange = onOrientationChange |. handle;
              let onVisibilityChange = onVisibilityChange |. handle;

              let subscriptions =
                Subscriptions.{
                  install: () => {
                    Events.subscribeToTouchMove(onTouchMove);
                    Events.subscribeToTouchEnd(onTouchEnd);
                    Events.subscribeToContextMenu(onContextMenu);
                    Events.subscribeToOrientationChange(onOrientationChange);
                    Events.subscribeToVisibilityChange(onVisibilityChange);
                  },
                  drop: () => {
                    Events.unsubscribeFromTouchMove(onTouchMove);
                    Events.unsubscribeFromTouchEnd(onTouchEnd);
                    Events.unsubscribeFromContextMenu(onContextMenu);
                    Events.unsubscribeFromOrientationChange(
                      onOrientationChange,
                    );
                    Events.unsubscribeFromVisibilityChange(
                      onVisibilityChange,
                    );
                  },
                };

              state.context.initDrag(
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
          | Some(timeoutId) => timeoutId |> Js.Global.clearTimeout
          | None => ()
          }
        and onInitialTouchMove = _ => cancelDrag()
        and onInitialTouchEnd = _ => cancelDrag()
        and onInitialDrag = _ => cancelDrag()
        and dropInitialSubscriptions = () => {
          Events.unsubscribeFromTouchMove(onInitialTouchMove);
          Events.unsubscribeFromTouchEnd(onInitialTouchEnd);
          Events.unsubscribeFromDrag(onInitialDrag);
        };

        Events.subscribeToTouchMove(onInitialTouchMove);
        Events.subscribeToTouchEnd(onInitialTouchEnd);
        Events.subscribeToDrag(onInitialDrag);

        timeoutId := startDragging() |. Some;

      | (StandBy, _, _)
      | (Dragging(_, _), _, _)
      | (Dropping(_), _, _) => ()
      };
    }
    and onTouchMove = (event, {ReasonReact.state}) => {
      open Webapi.Dom;

      let event = event |. Events.Touch.castEventToTouchEvent;
      let touch =
        event
        |. TouchEvent.touches
        |. Events.Touch.castDomTouchListToTouchArray
        |. Array.get(0);

      switch (state.context.status, state.element^, touch) {
      | (Dragging(ghost, _), Some(_), Some(touch))
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event |. TouchEvent.preventDefault;

        let point =
          RelativityBag.{
            page: Point.{x: touch##pageX, y: touch##pageY},
            viewport: Point.{x: touch##clientX, y: touch##clientY},
          };

        state.context.updateGhostPosition(point);

      | (Dragging(_, _), _, _)
      | (Dropping(_), _, _)
      | (StandBy, _, _) => ()
      };
    }
    and onTouchEnd = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event |> Webapi.Dom.Event.preventDefault;
        subscriptions.drop();
        state.context.drop();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onContextMenu = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        event |> Webapi.Dom.Event.preventDefault
      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onOrientationChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        subscriptions.drop();
        state.context.cancelDrag();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when Cfg.Draggable.eq(state.draggableId, ghost.draggableId) =>
        subscriptions.drop();
        state.context.cancelDrag();

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      };
  };

  type dragHandle = {
    style: ReactDOMRe.Style.t,
    onMouseDown: ReactEventRe.Mouse.t => unit,
    onTouchStart: ReactEventRe.Touch.t => unit,
  };

  type children =
    | Children(ReasonReact.reactElement)
    | ChildrenWithDragHandle(dragHandle => ReasonReact.reactElement);

  let component = ReasonReact.reducerComponent("DndDraggable");
  let make =
      (
        ~id as draggableId: Cfg.Draggable.t,
        ~droppableId: Cfg.Droppable.t,
        ~context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
        ~className: option((~dragging: bool) => string)=?,
        children,
      ) => {
    ...component,
    initialState: () => {
      draggableId,
      droppableId,
      context,
      element: ref(None),
    },
    didMount: ({send, handle, onUnmount}) => {
      RegisterDraggable |> send;

      /* HACK: We have to add persistent event listener due to webkit bug:
       *       https://bugs.webkit.org/show_bug.cgi?id=184250
       */
      let preventTouchMoveInWebkit = (event, {ReasonReact.state}) =>
        Webapi.Dom.(
          switch (state.context.status) {
          | Dragging(ghost, _)
              when
                Cfg.Draggable.eq(state.draggableId, ghost.draggableId)
                && ! (event |> Event.defaultPrevented) =>
            event |. Event.preventDefault
          | _ => ()
          }
        );
      let handler = preventTouchMoveInWebkit |> handle;

      Events.subscribeToTouchMove(handler);
      onUnmount(() => Events.unsubscribeFromTouchMove(handler));
    },
    willReceiveProps: ({state}) => {
      ...state,
      draggableId,
      droppableId,
      context,
    },
    didUpdate: ({oldSelf, newSelf}) =>
      switch (oldSelf.state.context.status, newSelf.state.context.status) {
      | (Dropping(_), StandBy) => RegisterDraggable |> newSelf.send
      | _ => ()
      },
    willUnmount: _ => context.disposeDraggable(draggableId),
    reducer: (action, _) =>
      switch (action) {
      | RegisterDraggable =>
        ReasonReact.SideEffects(
          (
            self =>
              context.registerDraggable({
                id: draggableId,
                droppableId,
                getGeometry: () => self |> Handlers.getGeometry,
              })
          ),
        )
      },
    render: ({state, handle}) => {
      let setElementRef = element =>
        state.element :=
          element
          |. Js.Nullable.toOption
          |. Option.map(Webapi.Dom.Element.unsafeAsHtmlElement);

      let dragHandle = {
        style:
          ReactDOMRe.Style.make()
          |. ReactDOMRe.Style.unsafeAddProp(
               "WebkitTapHighlightColor",
               "rgba(0, 0, 0, 0)",
             ),
        onMouseDown: Handlers.onMouseDown |. handle,
        onTouchStart: Handlers.onTouchStart |. handle,
      };

      let children' =
        switch (children) {
        | Children(children) => [|children|]
        | ChildrenWithDragHandle(children) => [|dragHandle |. children|]
        };

      switch (context.status) {
      | Dragging(ghost, _)
          when Cfg.Draggable.eq(draggableId, ghost.draggableId) =>
        <Fragment>
          (
            ReasonReact.createDomElement(
              "div",
              ~props={
                "ref": setElementRef,
                "style":
                  ReactDOMRe.Style.make(
                    ~position="fixed",
                    ~boxSizing="border-box",
                    ~zIndex="10000",
                    ~margin="0",
                    ~overflow="visible",
                    ~pointerEvents="none",
                    ~userSelect="none",
                    ~top=Style.(ghost.departureRect.page.top |. px),
                    ~left=Style.(ghost.departureRect.page.left |. px),
                    ~width=Style.(ghost.dimensions.width |. px),
                    ~height=Style.(ghost.dimensions.height |. px),
                    ~transform=
                      Style.translate(
                        ghost.delta.x
                        - (
                          switch (context.scroll) {
                          | Some(scroll) => scroll.current.x
                          | None => Webapi.Dom.(window |> Window.pageXOffset)
                          }
                        ),
                        ghost.delta.y
                        - (
                          switch (context.scroll) {
                          | Some(scroll) => scroll.current.y
                          | None => Webapi.Dom.(window |> Window.pageYOffset)
                          }
                        ),
                      ),
                    (),
                  )
                  |. ReactDOMRe.Style.unsafeAddProp(
                       "WebkitUserSelect",
                       "none",
                     ),
                "className":
                  className
                  |. Option.map(fn => fn(~dragging=true))
                  |. Js.Nullable.fromOption,
              },
              children',
            )
          )
          <div
            style=(
              ReactDOMRe.Style.make(
                ~boxSizing="border-box",
                ~marginTop=Style.(ghost.margins.top |. px),
                ~marginBottom=Style.(ghost.margins.bottom |. px),
                ~marginLeft=Style.(ghost.margins.left |. px),
                ~marginRight=Style.(ghost.margins.right |. px),
                ~width=Style.(0 |. px),
                ~height=Style.(ghost.dimensions.height |. px),
                ~transition=Style.transition("all"),
                (),
              )
            )
          />
        </Fragment>

      | Dropping(ghost) when Cfg.Draggable.eq(draggableId, ghost.draggableId) =>
        <Fragment>
          (
            ReasonReact.createDomElement(
              "div",
              ~props={
                "ref": setElementRef,
                "style":
                  ReactDOMRe.Style.make(
                    ~position="fixed",
                    ~boxSizing="border-box",
                    ~zIndex="10000",
                    ~margin="0",
                    ~overflow="visible",
                    ~pointerEvents="none",
                    ~userSelect="none",
                    ~top=Style.(ghost.departureRect.page.top |. px),
                    ~left=Style.(ghost.departureRect.page.left |. px),
                    ~width=Style.(ghost.dimensions.width |. px),
                    ~height=Style.(ghost.dimensions.height |. px),
                    ~transition=Style.transition("transform"),
                    ~transform=
                      Style.translate(
                        ghost.delta.x
                        - (
                          switch (context.scroll) {
                          | Some(scroll) => scroll.current.x
                          | None => Webapi.Dom.(window |> Window.pageXOffset)
                          }
                        ),
                        ghost.delta.y
                        - (
                          switch (context.scroll) {
                          | Some(scroll) => scroll.current.y
                          | None => Webapi.Dom.(window |> Window.pageYOffset)
                          }
                        ),
                      ),
                    (),
                  )
                  |. ReactDOMRe.Style.unsafeAddProp(
                       "WebkitUserSelect",
                       "none",
                     ),
                "className":
                  className
                  |. Option.map(fn => fn(~dragging=false))
                  |. Js.Nullable.fromOption,
              },
              children',
            )
          )
          <div
            style=(
              ReactDOMRe.Style.make(
                ~boxSizing="border-box",
                ~marginTop=Style.(ghost.margins.top |. px),
                ~marginBottom=Style.(ghost.margins.bottom |. px),
                ~marginLeft=Style.(ghost.margins.left |. px),
                ~marginRight=Style.(ghost.margins.right |. px),
                ~width=Style.(0 |. px),
                ~height=Style.(ghost.dimensions.height |. px),
                ~transition=Style.transition("all"),
                (),
              )
            )
          />
        </Fragment>

      | Dragging(ghost, _)
      | Dropping(ghost) =>
        switch (draggableId |. context.getDraggableShift) {
        | Some(Alpha) when ghost.targetingOriginalDroppable =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style":
                ReactDOMRe.Style.make(
                  ~boxSizing="border-box",
                  ~pointerEvents="none",
                  ~userSelect="none",
                  ~transition=Style.transition("transform"),
                  ~transform=
                    Style.translate(
                      0,
                      - (
                        ghost.dimensions.height
                        + ghost.margins.top
                        + ghost.margins.bottom
                      ),
                    ),
                  (),
                )
                |. ReactDOMRe.Style.unsafeAddProp("WebkitUserSelect", "none"),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )

        | Some(Omega) when ghost.targetingOriginalDroppable =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style":
                ReactDOMRe.Style.make(
                  ~boxSizing="border-box",
                  ~pointerEvents="none",
                  ~userSelect="none",
                  ~transition=Style.transition("transform"),
                  ~transform=
                    Style.translate(
                      0,
                      ghost.dimensions.height
                      + ghost.margins.top
                      + ghost.margins.bottom,
                    ),
                  (),
                )
                |. ReactDOMRe.Style.unsafeAddProp("WebkitUserSelect", "none"),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )

        | Some(Alpha) =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style":
                ReactDOMRe.Style.make(
                  ~boxSizing="border-box",
                  ~pointerEvents="none",
                  ~userSelect="none",
                  ~transition=Style.transition("transform"),
                  (),
                )
                |. ReactDOMRe.Style.unsafeAddProp("WebkitUserSelect", "none"),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )

        | Some(Omega) =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style":
                ReactDOMRe.Style.make(
                  ~boxSizing="border-box",
                  ~pointerEvents="none",
                  ~userSelect="none",
                  ~transition=Style.transition("transform"),
                  ~transform=
                    Style.translate(
                      0,
                      ghost.dimensions.height
                      + ghost.margins.top
                      + ghost.margins.bottom,
                    ),
                  (),
                )
                |. ReactDOMRe.Style.unsafeAddProp("WebkitUserSelect", "none"),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )

        | None =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style":
                ReactDOMRe.Style.make(
                  ~boxSizing="border-box",
                  ~pointerEvents="none",
                  ~userSelect="none",
                  ~transition=Style.transition("transform"),
                  (),
                )
                |. ReactDOMRe.Style.unsafeAddProp("WebkitUserSelect", "none"),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )
        }

      | StandBy =>
        switch (children) {
        | Children(_) =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style": ReactDOMRe.Style.make(~boxSizing="border-box", ()),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
              "onMouseDown": dragHandle.onMouseDown,
              "onTouchStart": dragHandle.onTouchStart,
            },
            children',
          )
        | ChildrenWithDragHandle(_) =>
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": setElementRef,
              "style": ReactDOMRe.Style.make(~boxSizing="border-box", ()),
              "className":
                className
                |. Option.map(fn => fn(~dragging=false))
                |. Js.Nullable.fromOption,
            },
            children',
          )
        }
      };
    },
  };
};
