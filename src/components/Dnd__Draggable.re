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
    open Webapi.Dom;

    let rec onMouseDown = (event, {ReasonReact.state, handle}) =>
      switch (state.context.status, state.element^, event |. Events.leftClick) {
      | (StandBy, Some(element), true) =>
        let moveThreshold = 1;

        let startX = event |. ReactEventRe.Mouse.pageX;
        let startY = event |. ReactEventRe.Mouse.pageY;

        let rec onInitialMouseMove = event => {
          let currentX = event |. MouseEvent.pageX;
          let currentY = event |. MouseEvent.pageY;

          let initiateDrag =
            Js.Math.abs_int(startX - currentX) > moveThreshold
            || Js.Math.abs_int(startY - currentY) > moveThreshold;

          if (initiateDrag) {
            dropInitialSubscriptions();

            /* Clear text selection */
            switch (document |> Document.asHtmlDocument) {
            | Some(document) =>
              document
              |> HtmlDocument.getSelection
              |> Selection.removeAllRanges
            | None => ()
            };

            let rect = element |. HtmlElement.getBoundingClientRect;
            let style =
              element
              |. Html.castHtmlElementToElement
              |. Window.getComputedStyle(window);

            let ghost =
              Ghost.{
                draggableId: state.draggableId,
                originalDroppable: state.droppableId,
                targetDroppable: Some(state.droppableId),
                targetingOriginalDroppable: true,
                direction: Geometry.getDirection(startY, currentY),
                dimensions: rect |. Geometry.getDimensions,
                margins: style |. Geometry.getMargins,
                borders: style |. Geometry.getBorders,
                center: rect |. Geometry.getAbsCenter,
                departureRect: rect |. Geometry.getAbsRect,
                currentRect: rect |. Geometry.getAbsRect,
                departurePoint: {
                  x: currentX,
                  y: currentY,
                },
                currentPoint: {
                  x: currentX,
                  y: currentY,
                },
                delta: {
                  x: 0,
                  y: 0,
                },
              };

            let onMouseMove = onMouseMove |. handle;
            let onMouseUp = onMouseUp |. handle;
            let onKeyDown = onKeyDown |. handle;

            let subscriptions =
              Subscriptions.{
                install: () => {
                  Events.subscribeToMouseMove(onMouseMove);
                  Events.subscribeToMouseUp(onMouseUp);
                  Events.subscribeToKeyDown(onKeyDown);
                },
                drop: () => {
                  Events.unsubscribeFromMouseMove(onMouseMove);
                  Events.unsubscribeFromMouseUp(onMouseUp);
                  Events.unsubscribeFromKeyDown(onKeyDown);
                },
              };

            state.context.startDragging(ghost, subscriptions);
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

      | (_, _, false) => ()
      | (StandBy, None, _)
      | (Dropping(_), _, _)
      | (Dragging(_, _), _, _) => ()
      }
    and onMouseMove = (event, {ReasonReact.state}) =>
      switch (state.context.status, state.element^) {
      | (Dragging(ghost, subscriptions), Some(element))
          when state.draggableId == ghost.draggableId =>
        event |. MouseEvent.preventDefault;

        let currentX = event |. MouseEvent.pageX;
        let currentY = event |. MouseEvent.pageY;

        let rect = element |. HtmlElement.getBoundingClientRect;

        let ghost = {
          ...ghost,
          direction: Geometry.getDirection(ghost.currentPoint.y, currentY),
          center: rect |. Geometry.getAbsCenter,
          currentRect: rect |. Geometry.getAbsRect,
          currentPoint: {
            x: currentX,
            y: currentY,
          },
          delta: {
            x: currentX - ghost.departurePoint.x,
            y: currentY - ghost.departurePoint.y,
          },
        };

        state.context.updateGhostPosition(ghost, subscriptions);

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
                  ~width=Style.(ghost.dimensions.width |. px),
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
