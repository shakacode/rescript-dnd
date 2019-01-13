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
    let getGeometryAndScrollable = ({React.state}) => {
      open Webapi.Dom;

      let el = (state.element^)->Option.getExn;

      let elRect = el->HtmlElement.getBoundingClientRect;
      let elStyle = el->Style.getComputedStyle;
      let windowScrollPosition = Scrollable.Window.getScrollPosition();

      let geometry =
        Geometry.getGeometry(elRect, elStyle, windowScrollPosition);

      let scrollable =
        if (elStyle->Scrollable.Element.isScrollable) {
          let elMaxScroll = el->Scrollable.Element.getMaxScroll;
          let elScrollPosition = el->Scrollable.Element.getScrollPosition;

          Some(
            ScrollableElement.{
              element: el,
              geometry,
              scroll:
                Scroll.{
                  max: elMaxScroll,
                  initial: elScrollPosition,
                  current: elScrollPosition,
                  delta: {
                    x: 0.,
                    y: 0.,
                  },
                },
            },
          );
        } else {
          el->Scrollable.Element.getClosestScrollable;
        };

      (geometry, scrollable);
    };
  };

  let component = React.reducerComponent("DndDroppable");
  let make =
      (
        ~id as droppableId: Cfg.Droppable.t,
        ~axis: Axis.t,
        ~accept: option(Cfg.Draggable.t => bool)=?,
        ~context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
        ~className: option((~draggingOver: bool) => string)=?,
        children,
      ) => {
    ...component,
    initialState: () => {context, element: None->ref},
    didMount: ({send}) => RegisterDroppable->send,
    willReceiveProps: ({state}) => {...state, context},
    didUpdate: ({oldSelf, newSelf}) =>
      switch (oldSelf.state.context.status, newSelf.state.context.status) {
      | (Dropping(_), StandBy) => RegisterDroppable->(newSelf.send)
      | _ => ()
      },
    willUnmount: _ => context.disposeDroppable(droppableId),
    reducer: (action, _) =>
      switch (action) {
      | RegisterDroppable =>
        React.SideEffects(
          self =>
            context.registerDroppable({
              id: droppableId,
              axis,
              accept,
              getGeometryAndScrollable: () =>
                self->Handlers.getGeometryAndScrollable,
            }),
        )
      },
    render: ({state}) =>
      <div
        ref={el =>
          state.element :=
            el
            ->Js.Nullable.toOption
            ->Option.map(Webapi.Dom.Element.unsafeAsHtmlElement)
        }
        className=?{
          className->Option.map(fn =>
            fn(
              ~draggingOver=
                context.target
                ->Option.map(target => target->Cfg.Droppable.eq(droppableId))
                ->Option.getWithDefault(false),
            )
          )
        }>
        {switch (context.status) {
         | Dragging(ghost, _)
         | Dropping(ghost)
             when
               Option.eq(
                 ghost.targetDroppable,
                 Some(droppableId),
                 Cfg.Droppable.eq,
               )
               && !ghost.targetingOriginalDroppable =>
           let (width, height) =
             switch (ghost.axis) {
             | X => Style.(ghost.dimensions.width->px, 0.->px)
             | Y => Style.(0.->px, ghost.dimensions.height->px)
             };

           children
           ->Array.concat([|
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
                   ~borderTop=Style.(ghost.borders.top->px),
                   ~borderBottom=Style.(ghost.borders.bottom->px),
                   ~borderLeft=Style.(ghost.borders.left->px),
                   ~borderRight=Style.(ghost.borders.right->px),
                   ~transition=Style.transition("all"),
                   (),
                 )}
               />,
             |])
           ->React.array;

         | Moving(pawn, _)
             when
               Cfg.Droppable.eq(pawn.targetDroppable, droppableId)
               && !pawn.targetingOriginalDroppable =>
           let (width, height) =
             switch (pawn.axis) {
             | X => Style.(pawn.dimensions.width->px, 0.->px)
             | Y => Style.(0.->px, pawn.dimensions.height->px)
             };

           children
           ->Array.concat([|
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
                   ~borderTop=Style.(pawn.borders.top->px),
                   ~borderBottom=Style.(pawn.borders.bottom->px),
                   ~borderLeft=Style.(pawn.borders.left->px),
                   ~borderRight=Style.(pawn.borders.right->px),
                   ~transition=Style.transition("all"),
                   (),
                 )}
               />,
             |])
           ->React.array;

         | _ =>
           children
           ->Array.concat([|
               <div
                 style={ReactDom.Style.make(
                   ~boxSizing="border-box",
                   ~margin=Style.(0.->px),
                   ~border=Style.(0.->px),
                   ~width=Style.(0.->px),
                   ~height=Style.(0.->px),
                   ~transition="none",
                   (),
                 )}
               />,
             |])
           ->React.array
         }}
      </div>,
  };
};
