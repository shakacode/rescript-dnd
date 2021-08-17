open Dnd__Types;

module Context = Dnd__DndContext;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;
module Scrollable = Dnd__Scrollable;
module ReactHooks = Dnd__ReactHooks;

module Make = (Context: Context.T) => {
  module Item = Context.Item;
  module Container = Context.Container;

  module Helpers = {
    let getGeometryAndScrollable = element => {
      open Webapi.Dom;

      let elementRect = element->HtmlElement.getBoundingClientRect;
      let elementStyle = element->Style.getComputedStyle;
      let windowScrollPosition = Scrollable.Window.getScrollPosition();

      let geometry =
        Geometry.getGeometry(elementRect, elementStyle, windowScrollPosition);

      let scrollable =
        if (elementStyle->Scrollable.Element.isScrollable) {
          let elementMaxScroll = element->Scrollable.Element.getMaxScroll;
          let elementScrollPosition =
            element->Scrollable.Element.getScrollPosition;

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
                    x: 0.,
                    y: 0.,
                  },
                },
            },
          );
        } else {
          element->Scrollable.Element.getClosestScrollable;
        };

      (geometry, scrollable);
    };
  };

  [@react.component]
  let make =
      (
        ~id as containerId: Container.t,
        ~axis: Axis.t,
        ~lockAxis: bool=false,
        ~accept: option(Item.t => bool)=?,
        ~className: option((~draggingOver: bool) => string)=?,
        ~children,
      ) => {
    let ctx = React.useContext(Context.x);
    let element = React.useRef(Js.Nullable.null);
    let prevStatus = ReactHooks.usePrevious(ctx.status);

    React.useEffect2(
      () =>
        switch (prevStatus, ctx.status) {
        | (Some(StandBy), Collecting(_)) =>
          ctx.registerContainer({
            id: containerId,
            axis,
            lockAxis,
            accept,
            element:
              element.current
              ->Js.Nullable.toOption
              ->Option.getExn
              ->Webapi.Dom.Element.unsafeAsHtmlElement,
            getGeometryAndScrollable: () =>
              element.current
              ->Js.Nullable.toOption
              ->Option.getExn
              ->Webapi.Dom.Element.unsafeAsHtmlElement
              ->Helpers.getGeometryAndScrollable,
          });
          None;
        | (
            Some(_) | None,
            StandBy | Collecting(_) | Dragging(_) | Dropping(_),
          ) =>
          None
        },
      (prevStatus, ctx.status),
    );

    <div
      ref={element->ReactDOM.Ref.domRef}
      className=?{
        className->Option.map(fn =>
          fn(
            ~draggingOver=
              ctx.target
              ->Option.map(target => target->Container.eq(containerId))
              ->Option.getWithDefault(false),
          )
        )
      }>
      {switch (ctx.status) {
       | Dragging(ghost, _)
       | Dropping(ghost, _)
           when
             Option.eq(ghost.targetContainer, containerId->Some, Container.eq)
             && !ghost.targetingOriginalContainer =>
         let (width, height) =
           switch (ghost.axis) {
           | X => Style.(ghost.dimensions.width->px, 0.->px)
           | Y => Style.(0.->px, ghost.dimensions.height->px)
           };

         <>
           children
           <div
             style={ReactDOM.Style.make(
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
           />
         </>;

       | StandBy
       | Collecting(_)
       | Dragging(_)
       | Dropping(_) =>
         <>
           children
           <div
             style={ReactDOM.Style.make(
               ~boxSizing="border-box",
               ~margin=Style.(0.->px),
               ~border=Style.(0.->px),
               ~width=Style.(0.->px),
               ~minWidth=Style.(0.->px),
               ~height=Style.(0.->px),
               ~minHeight=Style.(0.->px),
               ~transition=Style.transition("all"),
               (),
             )}
           />
         </>
       }}
    </div>;
  };
};
