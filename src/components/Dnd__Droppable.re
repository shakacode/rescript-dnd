open Dnd__React;
open Dnd__Config;
open Dnd__Types;

module Html = Dnd__Html;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;
module Scrollable = Dnd__Scrollable;

module Make = (Cfg: Config) => {
  type state = {
    context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
    element: ref(option(Dom.htmlElement)),
  };

  type action =
    | RegisterDroppable;

  module Handlers = {
    let getGeometryAndScrollable = ({ReasonReact.state}) => {
      open Webapi.Dom;

      let element = state.element^ |> Option.getExn;

      let elementRect = element |> HtmlElement.getBoundingClientRect;
      let elementStyle = element |> Style.getComputedStyle;
      let windowScrollPosition = Scrollable.Window.getScrollPosition();

      let geometry =
        Geometry.getGeometry(elementRect, elementStyle, windowScrollPosition);

      let scrollable =
        if (elementStyle |> Scrollable.Element.isScrollable) {
          let elementMaxScroll = element |> Scrollable.Element.getMaxScroll;
          let elementScrollPosition =
            element |> Scrollable.Element.getScrollPosition;

          Some(
            ScrollableElement.{
              element,
              geometry,
              scroll:
                Scroll.{
                  max: elementMaxScroll,
                  initial: elementScrollPosition,
                  current: elementScrollPosition,
                  delta: {
                    x: 0,
                    y: 0,
                  },
                },
            },
          );
        } else {
          element |> Scrollable.Element.getClosestScrollable;
        };

      (geometry, scrollable);
    };
  };

  let component = ReasonReact.reducerComponent("DndDroppable");
  let make =
      (
        ~id as droppableId: Cfg.Droppable.t,
        ~accept: option(Cfg.Draggable.t => bool)=?,
        ~context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
        ~className: option((~draggingOver: bool) => string)=?,
        children,
      ) => {
    ...component,
    initialState: () => {context, element: ref(None)},
    didMount: ({send}) => RegisterDroppable |> send,
    willReceiveProps: ({state}) => {...state, context},
    didUpdate: ({oldSelf, newSelf}) =>
      switch (oldSelf.state.context.status, newSelf.state.context.status) {
      | (Dropping(_), StandBy) => RegisterDroppable |> newSelf.send
      | _ => ()
      },
    willUnmount: _ => context.disposeDroppable(droppableId),
    reducer: (action, _) =>
      switch (action) {
      | RegisterDroppable =>
        ReasonReact.SideEffects(
          (
            self =>
              context.registerDroppable({
                id: droppableId,
                accept,
                getGeometryAndScrollable: () =>
                  self |> Handlers.getGeometryAndScrollable,
              })
          ),
        )
      },
    render: ({state}) =>
      <Fragment>
        (
          ReasonReact.createDomElement(
            "div",
            ~props={
              "ref": element =>
                state.element :=
                  element
                  |. Js.Nullable.toOption
                  |. Option.map(Webapi.Dom.Element.unsafeAsHtmlElement),
              "className":
                className
                |. Option.map(fn =>
                     fn(
                       ~draggingOver=
                         context.target
                         |. Option.map(target =>
                              Cfg.Droppable.eq(target, droppableId)
                            )
                         |. Option.getWithDefault(false),
                     )
                   )
                |. Js.Nullable.fromOption,
            },
            switch (context.status) {
            | Dragging(ghost, _)
            | Dropping(ghost)
                when
                  Option.eq(
                    ghost.targetDroppable,
                    Some(droppableId),
                    Cfg.Droppable.eq,
                  )
                  && ! ghost.targetingOriginalDroppable =>
              children
              |. Array.concat([|
                   <div
                     style=(
                       ReactDOMRe.Style.make(
                         ~boxSizing="border-box",
                         ~marginTop=Style.(ghost.margins.top |. px),
                         ~marginBottom=Style.(ghost.margins.bottom |. px),
                         ~marginLeft=Style.(ghost.margins.left |. px),
                         ~marginRight=Style.(ghost.margins.right |. px),
                         ~borderTop=Style.(ghost.borders.top |. px),
                         ~borderBottom=Style.(ghost.borders.bottom |. px),
                         ~borderLeft=Style.(ghost.borders.left |. px),
                         ~borderRight=Style.(ghost.borders.right |. px),
                         ~width=Style.(0 |. px),
                         ~height=Style.(ghost.dimensions.height |. px),
                         ~transition=Style.transition("all"),
                         (),
                       )
                     )
                   />,
                 |])
            | _ =>
              children
              |. Array.concat([|
                   <div
                     style=(
                       ReactDOMRe.Style.make(
                         ~boxSizing="border-box",
                         ~margin=Style.(0 |. px),
                         ~border=Style.(0 |. px),
                         ~width=Style.(0 |. px),
                         ~height=Style.(0 |. px),
                         ~transition="none",
                         (),
                       )
                     )
                   />,
                 |])
            },
          )
        )
      </Fragment>,
  };
};
