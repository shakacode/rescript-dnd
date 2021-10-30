open Dnd__Types

module Context = Dnd__DndContext
module Style = Dnd__Style
module Geometry = Dnd__Geometry
module Scrollable = Dnd__Scrollable
module ReactHooks = Dnd__ReactHooks

module Make = (Context: Context.T) => {
  module Item = Context.Item
  module Container = Context.Container

  module Helpers = {
    let getGeometryAndScrollable = element => {
      open Webapi.Dom

      let elementRect = element->HtmlElement.getBoundingClientRect
      let elementStyle = element->Style.getComputedStyle
      let windowScrollPosition = Scrollable.Window.getScrollPosition()

      let geometry = Geometry.getGeometry(elementRect, elementStyle, windowScrollPosition)

      let scrollable = if elementStyle->Scrollable.Element.isScrollable {
        let elementMaxScroll = element->Scrollable.Element.getMaxScroll
        let elementScrollPosition = element->Scrollable.Element.getScrollPosition

        Some({
          open ScrollableElement
          {
            element: element,
            geometry: geometry,
            scroll: {
              open Scroll
              {
                max: elementMaxScroll,
                initial: elementScrollPosition,
                current: elementScrollPosition,
                delta: {
                  x: 0.,
                  y: 0.,
                },
              }
            },
          }
        })
      } else {
        element->Scrollable.Element.getClosestScrollable
      }

      (geometry, scrollable)
    }
  }

  @react.component
  let make = (
    ~id as containerId: Container.t,
    ~axis: Axis.t,
    ~lockAxis: bool=false,
    ~accept: option<Item.t => bool>=?,
    ~className: option<(~draggingOver: bool) => string>=?,
    ~children,
  ) => {
    let ctx = React.useContext(Context.x)
    let element = React.useRef(Js.Nullable.null)
    let prevStatus = ReactHooks.usePrevious(ctx.status)

    React.useEffect2(() =>
      switch (prevStatus, ctx.status) {
      | (Some(StandBy), Collecting(_)) =>
        %log.debug(
          "RegisterItem"
          ("ContainerId", containerId)
        )
        ctx.registerContainer({
          id: containerId,
          axis: axis,
          lockAxis: lockAxis,
          accept: accept,
          element: element.current
          ->Js.Nullable.toOption
          ->Option.getExn
          ->Webapi.Dom.Element.unsafeAsHtmlElement,
          getGeometryAndScrollable: () =>
            element.current
            ->Js.Nullable.toOption
            ->Option.getExn
            ->Webapi.Dom.Element.unsafeAsHtmlElement
            ->Helpers.getGeometryAndScrollable,
        })
        None
      | (Some(_) | None, StandBy | Collecting(_) | Dragging(_) | Dropping(_)) => None
      }
    , (prevStatus, ctx.status))

    <div
      ref={element->ReactDOM.Ref.domRef}
      className=?{className->Option.map(fn =>
        fn(
          ~draggingOver=ctx.target
          ->Option.map(target => target->Container.eq(containerId))
          ->Option.getWithDefault(false),
        )
      )}>
      {switch ctx.status {
      | Dragging(ghost, _)
      | Dropping(ghost, _)
        if Option.eq(ghost.targetContainer, containerId->Some, Container.eq) &&
        !ghost.targetingOriginalContainer =>
        let (width, height) = switch ghost.axis {
        | X =>
          open Style
          (ghost.dimensions.width->px, 0.->px)
        | Y =>
          open Style
          (0.->px, ghost.dimensions.height->px)
        }

        <>
          children
          <div
            style={ReactDOM.Style.make(
              ~boxSizing="border-box",
              ~width,
              ~minWidth=width,
              ~height,
              ~minHeight=height,
              ~marginTop={
                open Style
                ghost.margins.top->px
              },
              ~marginBottom={
                open Style
                ghost.margins.bottom->px
              },
              ~marginLeft={
                open Style
                ghost.margins.left->px
              },
              ~marginRight={
                open Style
                ghost.margins.right->px
              },
              ~borderTop={
                open Style
                ghost.borders.top->px
              },
              ~borderBottom={
                open Style
                ghost.borders.bottom->px
              },
              ~borderLeft={
                open Style
                ghost.borders.left->px
              },
              ~borderRight={
                open Style
                ghost.borders.right->px
              },
              ~transition=Style.transition("all"),
              (),
            )}
          />
        </>

      | StandBy
      | Collecting(_)
      | Dragging(_)
      | Dropping(_) => <>
          children
          <div
            style={ReactDOM.Style.make(
              ~boxSizing="border-box",
              ~margin={
                open Style
                0.->px
              },
              ~border={
                open Style
                0.->px
              },
              ~width={
                open Style
                0.->px
              },
              ~minWidth={
                open Style
                0.->px
              },
              ~height={
                open Style
                0.->px
              },
              ~minHeight={
                open Style
                0.->px
              },
              ~transition=Style.transition("all"),
              (),
            )}
          />
        </>
      }}
    </div>
  }
}
