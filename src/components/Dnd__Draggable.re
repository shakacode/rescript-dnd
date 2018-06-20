open Dnd__React;
open Dnd__Config;
open Dnd__Types;

module Html = Dnd__Html;
module Events = Dnd__Events;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;

module Make = (Config: Config) => {
  type state = {
    draggableId: Config.draggableId,
    droppableId: Config.droppableId,
    context: Context.t(Config.draggableId, Config.droppableId),
    element: ref(option(Dom.htmlElement)),
  };

  module Handlers = {
    let rec onMouseDown = (event, {ReasonReact.state, handle}) =>
      switch (state.context.status, state.element^) {
      | (StandBy, Some(element))
          when event |. Events.leftClick && ! (event |. Events.modifier) =>
        open Webapi.Dom;

        let moveThreshold = 1;

        let start =
          Point.{
            x: event |. ReactEventRe.Mouse.pageX,
            y: event |. ReactEventRe.Mouse.pageY,
          };

        let rec onInitialMouseMove = event => {
          let current =
            Point.{
              x: event |. MouseEvent.pageX,
              y: event |. MouseEvent.pageY,
            };

          let initiateDrag =
            Js.Math.abs_int(start.x - current.x) > moveThreshold
            || Js.Math.abs_int(start.y - current.y) > moveThreshold;

          if (initiateDrag) {
            dropInitialSubscriptions();
            Html.clearTextSelection();

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

      | (StandBy, Some(_))
      | (StandBy, None)
      | (Dragging(_, _), _)
      | (Dropping(_), _) => ()
      }
    and onMouseMove = (event, {ReasonReact.state}) =>
      switch (state.context.status, state.element^) {
      | (Dragging(ghost, subscriptions), Some(element))
          when state.draggableId == ghost.draggableId =>
        open Webapi.Dom;

        event |. MouseEvent.preventDefault;

        let point =
          Point.{x: event |. MouseEvent.pageX, y: event |. MouseEvent.pageY};

        state.context.updateGhostPosition(
          ~ghost,
          ~point,
          ~element,
          ~subscriptions,
        );

      | (Dragging(_, _), _)
      | (Dropping(_), _)
      | (StandBy, _) => ()
      }
    and onMouseUp = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when state.draggableId == ghost.draggableId =>
        subscriptions.drop();
        state.context.startDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onKeyDown = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions) when event |> Events.isDomEscKey =>
        subscriptions.drop();
        state.context.cancelDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onResize = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions) =>
        subscriptions.drop();
        state.context.cancelDropping(ghost);

      | Dropping(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when state.draggableId == ghost.draggableId =>
        subscriptions.drop();
        state.context.cancelDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      };

    let rec onTouchStart = (event, {ReasonReact.state, handle}) => {
      let touch =
        event
        |. ReactEventRe.Touch.touches
        |. Events.castReactTouchListToTouchArray
        |. Array.get(0);

      switch (state.context.status, state.element^, touch) {
      | (StandBy, Some(element), Some(touch)) =>
        let delay = 200;

        let start = Point.{x: touch##pageX, y: touch##pageY};

        let timeoutId: ref(option(Js.Global.timeoutId)) = ref(None);

        let rec startDragging = () =>
          Js.Global.setTimeout(
            () => {
              dropInitialSubscriptions();
              Html.clearTextSelection();

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

      let event = event |. Events.castEventToTouchEvent;
      let touch =
        event
        |. TouchEvent.touches
        |. Events.castDomTouchListToTouchArray
        |. Array.get(0);

      switch (state.context.status, state.element^, touch) {
      | (Dragging(ghost, subscriptions), Some(element), Some(touch))
          when state.draggableId == ghost.draggableId =>
        event |. TouchEvent.preventDefault;

        let point = Point.{x: touch##pageX, y: touch##pageY};

        state.context.updateGhostPosition(
          ~ghost,
          ~point,
          ~element,
          ~subscriptions,
        );

      | (Dragging(_, _), _, _)
      | (Dropping(_), _, _)
      | (StandBy, _, _) => ()
      };
    }
    and onTouchEnd = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when state.draggableId == ghost.draggableId =>
        event |> Webapi.Dom.Event.preventDefault;
        subscriptions.drop();
        state.context.startDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onContextMenu = (event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, _) when state.draggableId == ghost.draggableId =>
        event |> Webapi.Dom.Event.preventDefault
      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onOrientationChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when state.draggableId == ghost.draggableId =>
        subscriptions.drop();
        state.context.cancelDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
    and onVisibilityChange = (_event, {ReasonReact.state}) =>
      switch (state.context.status) {
      | Dragging(ghost, subscriptions)
          when state.draggableId == ghost.draggableId =>
        subscriptions.drop();
        state.context.cancelDropping(ghost);

      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      };
  };

  let component = ReasonReact.reducerComponent("DndDraggable");

  let make =
      (
        ~id as draggableId: Config.draggableId,
        ~droppableId: Config.droppableId,
        ~context,
        ~className: option(string)=?,
        children,
      ) => {
    ...component,
    initialState: () => {
      draggableId,
      droppableId,
      context,
      element: ref(None),
    },
    didMount: ({handle, onUnmount}) => {
      /* HACK: We have to add persistent event listener due to webkit bug:
       *       https://bugs.webkit.org/show_bug.cgi?id=184250
       */
      let preventTouchMoveInWebkit = (event, {ReasonReact.state}) =>
        Webapi.Dom.(
          switch (state.context.status) {
          | Dragging(ghost, _)
              when
                state.draggableId == ghost.draggableId
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
    willUnmount: _ => context.disposeDraggable(draggableId),
    reducer: ((), _) => ReasonReact.NoUpdate,
    render: ({state, handle}) =>
      <Fragment>
        (
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": element => {
                let element =
                  element
                  |. Js.Nullable.toOption
                  |. Option.map(Webapi.Dom.Element.unsafeAsHtmlElement);

                state.element := element;

                switch (element) {
                | Some(element) =>
                  context.registerDraggable((
                    draggableId,
                    droppableId,
                    element,
                  ))
                | None => ()
                };
              },
              "style":
                switch (context.status) {
                | Dragging(ghost, _) when draggableId == ghost.draggableId =>
                  ReactDOMRe.Style.make(
                    ~position="fixed",
                    ~boxSizing="border-box",
                    ~zIndex="10000",
                    ~margin="0",
                    ~overflow="visible",
                    ~pointerEvents="none",
                    ~top=Style.(ghost.departureRect.top |. px),
                    ~left=Style.(ghost.departureRect.left |. px),
                    ~width=Style.(ghost.dimensions.width |. px),
                    ~height=Style.(ghost.dimensions.height |. px),
                    ~transition="none",
                    ~transform=
                      Style.translate(
                        ghost.delta.x - Webapi.Dom.(window |> Window.scrollX),
                        ghost.delta.y - Webapi.Dom.(window |> Window.scrollY),
                      ),
                    (),
                  )
                | Dropping(ghost) when draggableId == ghost.draggableId =>
                  ReactDOMRe.Style.make(
                    ~position="fixed",
                    ~boxSizing="border-box",
                    ~zIndex="10000",
                    ~margin="0",
                    ~overflow="visible",
                    ~pointerEvents="none",
                    ~top=Style.(ghost.departureRect.top |. px),
                    ~left=Style.(ghost.departureRect.left |. px),
                    ~width=Style.(ghost.dimensions.width |. px),
                    ~height=Style.(ghost.dimensions.height |. px),
                    ~transition=Style.transition("transform"),
                    ~transform=
                      Style.translate(
                        ghost.delta.x - Webapi.Dom.(window |> Window.scrollX),
                        ghost.delta.y - Webapi.Dom.(window |> Window.scrollY),
                      ),
                    (),
                  )
                | Dragging(ghost, _)
                | Dropping(ghost) =>
                  switch (draggableId |. context.getDraggableShift) {
                  | Some(Alpha) when ghost.targetingOriginalDroppable =>
                    ReactDOMRe.Style.make(
                      ~boxSizing="border-box",
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
                  | Some(Omega) when ghost.targetingOriginalDroppable =>
                    ReactDOMRe.Style.make(
                      ~boxSizing="border-box",
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

                  | Some(Alpha) =>
                    ReactDOMRe.Style.make(
                      ~boxSizing="border-box",
                      ~transition=Style.transition("transform"),
                      (),
                    )
                  | Some(Omega) =>
                    ReactDOMRe.Style.make(
                      ~boxSizing="border-box",
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
                  | None =>
                    ReactDOMRe.Style.make(
                      ~boxSizing="border-box",
                      ~transition=Style.transition("transform"),
                      (),
                    )
                  }
                | StandBy =>
                  ReactDOMRe.Style.make(~boxSizing="border-box", ())
                },
              "className": className |. Js.Nullable.fromOption,
              "onMouseDown": Handlers.onMouseDown |. handle,
              "onTouchStart": Handlers.onTouchStart |. handle,
            },
            children,
          )
        )
        (
          switch (context.status) {
          | Dragging(ghost, _)
          | Dropping(ghost) when draggableId == ghost.draggableId =>
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

          | Dragging(_, _)
          | Dropping(_)
          | StandBy => ReasonReact.null
          }
        )
      </Fragment>,
  };
};
