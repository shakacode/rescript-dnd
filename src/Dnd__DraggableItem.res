open Dnd__Types

module Context = Dnd__DndContext
module Events = Dnd__Events
module Style = Dnd__Style
module Geometry = Dnd__Geometry
module Scrollable = Dnd__Scrollable
module ReactHooks = Dnd__ReactHooks

module Make = (Context: Context.T) => {
  module Item = Context.Item
  module Container = Context.Container

  module Helpers = {
    let clearSelection = () => {
      open Webapi.Dom
      switch window->Window.getSelection {
      | Some(selection) => Selection.removeAllRanges(selection)
      | None => ()
      }
    }

    let getGeometry = element =>
      Geometry.getGeometry(
        element->Webapi.Dom.HtmlElement.getBoundingClientRect,
        element->Style.getComputedStyle,
        Scrollable.Window.getScrollPosition(),
      )
  }

  module MouseInteractions = {
    let onMouseDown = (
      ~itemId: Item.t,
      ~containerId: Container.t,
      ~ctx: React.ref<Context.t>,
      event,
    ) =>
      switch ctx.current.status {
      | StandBy
        if {
          open Events.Mouse
          event->leftClick && !(event->modifier)
        } =>
        %log.debug(
          "MouseDown"
          ("ItemId", itemId)
        )

        let moveThreshold = 1.

        let start = {
          open RelativityBag
          {
            page: {
              open Point
              {
                x: {
                  open ReactEvent
                  event->Mouse.pageX->Float.fromInt
                },
                y: {
                  open ReactEvent
                  event->Mouse.pageY->Float.fromInt
                },
              }
            },
            viewport: {
              open Point
              {
                x: {
                  open ReactEvent
                  event->Mouse.clientX->Float.fromInt
                },
                y: {
                  open ReactEvent
                  event->Mouse.clientY->Float.fromInt
                },
              }
            },
          }
        }

        let rec onInitialMouseMove = event => {
          // It prevents default browser action on link drag
          // TODO: Prolly, requires preventDefault on mousedrag
          event->Webapi.Dom.MouseEvent.preventDefault

          let current = {
            open RelativityBag
            {
              page: {
                open Point
                {
                  x: {
                    open Webapi.Dom
                    event->MouseEvent.pageX->Float.fromInt
                  },
                  y: {
                    open Webapi.Dom
                    event->MouseEvent.pageY->Float.fromInt
                  },
                }
              },
              viewport: {
                open Point
                {
                  x: {
                    open Webapi.Dom
                    event->MouseEvent.clientX->Float.fromInt
                  },
                  y: {
                    open Webapi.Dom
                    event->MouseEvent.clientY->Float.fromInt
                  },
                }
              },
            }
          }

          let moved =
            Js.Math.abs_float(start.page.x -. current.page.x) > moveThreshold ||
              Js.Math.abs_float(start.page.y -. current.page.y) > moveThreshold

          if moved {
            %log.debug(
              "MouseDown::Moved"
              ("ItemId", itemId)
            )

            dropInitialSubscriptions()
            Helpers.clearSelection()

            ctx.current.startDragging(itemId, containerId, start, current, #Mouse)
          }
        }
        and onInitialMouseUp = _ => dropInitialSubscriptions()
        and onInitialDrag = _ => dropInitialSubscriptions()
        and dropInitialSubscriptions = () => {
          %log.debug(
            "DropInitialSubscriptions"
            ("ItemId", itemId)
          )
          onInitialMouseMove->Events.unsubscribeFromMouseMove
          onInitialMouseUp->Events.unsubscribeFromMouseUp
          onInitialDrag->Events.unsubscribeFromDrag
        }

        onInitialMouseMove->Events.subscribeToMouseMove
        onInitialMouseUp->Events.subscribeToMouseUp
        onInitialDrag->Events.subscribeToDrag

      | Collecting(_)
      | Dragging(_, _)
      | Dropping(_)
      | StandBy => ()
      }
  }

  module TouchInteractions = {
    let onTouchStart = (
      ~itemId: Item.t,
      ~containerId: Container.t,
      ~ctx: React.ref<Context.t>,
      event,
    ) =>
      switch ctx.current.status {
      | StandBy =>
        let delay = 200

        let touch =
          event
          ->ReactEvent.Touch.touches
          ->Events.Touch.castReactTouchListToTouchArray
          ->Array.getUnsafe(0)

        let start = {
          open RelativityBag
          {
            page: {
              open Point
              {x: touch["pageX"], y: touch["pageY"]}
            },
            viewport: {
              open Point
              {x: touch["clientX"], y: touch["clientY"]}
            },
          }
        }

        let timeoutId: ref<option<Js.Global.timeoutId>> = None->ref

        let rec startDragging = () => Js.Global.setTimeout(() => {
            dropInitialSubscriptions()
            Helpers.clearSelection()

            ctx.current.startDragging(itemId, containerId, start, start, #Touch)
          }, delay)
        and cancelDrag = () =>
          switch timeoutId.contents {
          | Some(timeoutId) => timeoutId->Js.Global.clearTimeout
          | None => ()
          }
        and onInitialTouchMove = _ => cancelDrag()
        and onInitialTouchEnd = _ => cancelDrag()
        and onInitialDrag = _ => cancelDrag()
        and dropInitialSubscriptions = () => {
          onInitialTouchMove->Events.unsubscribeFromTouchMove
          onInitialTouchEnd->Events.unsubscribeFromTouchEnd
          onInitialDrag->Events.unsubscribeFromDrag
        }

        onInitialTouchMove->Events.subscribeToTouchMove
        onInitialTouchEnd->Events.subscribeToTouchEnd
        onInitialDrag->Events.subscribeToDrag

        timeoutId := startDragging()->Some

      | Collecting(_)
      | Dragging(_, _)
      | Dropping(_) => ()
      }
  }

  type children = [
    | #Children(React.element)
    | #ChildrenWithDragHandle((
      ~style: ReactDOM.Style.t,
      ~onMouseDown: ReactEvent.Mouse.t => unit,
      ~onTouchStart: ReactEvent.Touch.t => unit,
    ) => React.element)
  ]

  @react.component
  let make = (
    ~id as itemId: Item.t,
    ~containerId: Container.t,
    ~index: int,
    ~className: option<(~dragging: bool) => string>=?,
    ~children,
  ) => {
    let ctx = React.useContext(Context.x)
    let ctxRef = React.useRef(ctx)

    React.useEffect(() => {
      ctxRef.current = ctx
      None
    })

    let element = React.useRef(Js.Nullable.null)

    let prevStatus = ReactHooks.usePrevious(ctx.status)

    React.useEffect2(() =>
      switch (prevStatus, ctx.status) {
      | (Some(StandBy), Collecting(_)) =>
        %log.debug(
          "RegisterItem"
          ("ItemId", itemId)
        )
        ctx.registerItem({
          id: itemId,
          containerId: containerId,
          index: index,
          element: element.current
          ->Js.Nullable.toOption
          ->Option.getExn
          ->Webapi.Dom.Element.unsafeAsHtmlElement,
          getGeometry: () =>
            element.current
            ->Js.Nullable.toOption
            ->Option.getExn
            ->Webapi.Dom.Element.unsafeAsHtmlElement
            ->Helpers.getGeometry,
        })
        None
      | (Some(_) | None, StandBy | Collecting(_) | Dragging(_) | Dropping(_)) => None
      }
    , (prevStatus, ctx.status))

    let dragHandleStyle = React.useMemo0(() =>
      ReactDOM.Style.make()->ReactDOM.Style.unsafeAddProp(
        "WebkitTapHighlightColor",
        "rgba(0, 0, 0, 0)",
      )
    )
    let onMouseDown = React.useCallback3(
      MouseInteractions.onMouseDown(~itemId, ~containerId, ~ctx=ctxRef),
      (containerId, ctxRef, element),
    )
    let onTouchStart = React.useCallback3(
      TouchInteractions.onTouchStart(~itemId, ~containerId, ~ctx=ctxRef),
      (containerId, ctxRef, element),
    )

    let children' = switch children {
    | #Children(children) => children
    | #ChildrenWithDragHandle(children) =>
      // TODO: className: (~dragging) => ...,
      children(~style=dragHandleStyle, ~onMouseDown, ~onTouchStart)
    }

    switch ctx.status {
    | Dragging(ghost, _) if itemId->Item.eq(ghost.itemId) =>
      let (width, height) = switch ghost.axis {
      | X =>
        open Style
        (ghost.dimensions.width->px, 0.->px)
      | Y =>
        open Style
        (0.->px, ghost.dimensions.height->px)
      }

      <>
        <div
          ref={element->ReactDOM.Ref.domRef}
          tabIndex={-1}
          style={ReactDOM.Style.make(
            ~position="fixed",
            ~boxSizing="border-box",
            ~zIndex="10000",
            ~margin="0",
            ~overflow="visible",
            ~pointerEvents="none",
            ~userSelect="none",
            ~top={
              open Style
              ghost.departureRect.page.top->px
            },
            ~left={
              open Style
              ghost.departureRect.page.left->px
            },
            ~width={
              open Style
              ghost.dimensions.width->px
            },
            ~height={
              open Style
              ghost.dimensions.height->px
            },
            ~transform=Style.translate(
              ghost.delta.x -.
              switch ctx.scroll {
              | Some(scroll) => scroll.current.x
              | None =>
                open Webapi.Dom
                window->Window.pageXOffset
              },
              ghost.delta.y -.
              switch ctx.scroll {
              | Some(scroll) => scroll.current.y
              | None =>
                open Webapi.Dom
                window->Window.pageYOffset
              },
            ),
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=true))}>
          children'
        </div>
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
            ~transition=Style.transition("all"),
            (),
          )}
        />
      </>

    | Dropping(ghost, _) if itemId->Item.eq(ghost.itemId) =>
      let (width, height) = switch ghost.axis {
      | X =>
        open Style
        (ghost.dimensions.width->px, 0.->px)
      | Y =>
        open Style
        (0.->px, ghost.dimensions.height->px)
      }

      <>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~position="fixed",
            ~boxSizing="border-box",
            ~zIndex="10000",
            ~margin="0",
            ~overflow="visible",
            ~pointerEvents="none",
            ~userSelect="none",
            ~top={
              open Style
              ghost.departureRect.page.top->px
            },
            ~left={
              open Style
              ghost.departureRect.page.left->px
            },
            ~width={
              open Style
              ghost.dimensions.width->px
            },
            ~height={
              open Style
              ghost.dimensions.height->px
            },
            ~transition=Style.transition("transform"),
            ~transform=Style.translate(
              ghost.delta.x -.
              switch ctx.scroll {
              | Some(scroll) => scroll.current.x
              | None =>
                open Webapi.Dom
                window->Window.pageXOffset
              },
              ghost.delta.y -.
              switch ctx.scroll {
              | Some(scroll) => scroll.current.y
              | None =>
                open Webapi.Dom
                window->Window.pageYOffset
              },
            ),
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=true))}>
          children'
        </div>
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
            ~transition=Style.transition("all"),
            (),
          )}
        />
      </>

    | Dragging(ghost, _)
    | Dropping(ghost, _) =>
      switch ctx.getItemShift(itemId) {
      | Some(Alpha) if ghost.targetingOriginalContainer =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~boxSizing="border-box",
            ~pointerEvents="none",
            ~userSelect="none",
            ~transition=Style.transition("transform"),
            ~transform=switch ghost.axis {
            | X =>
              Style.translate(
                -.(ghost.dimensions.width +. ghost.margins.left +. ghost.margins.right),
                0.,
              )
            | Y =>
              Style.translate(
                0.,
                -.(ghost.dimensions.height +. ghost.margins.top +. ghost.margins.bottom),
              )
            },
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>

      | Some(Omega) if ghost.targetingOriginalContainer =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~boxSizing="border-box",
            ~pointerEvents="none",
            ~userSelect="none",
            ~transition=Style.transition("transform"),
            ~transform=switch ghost.axis {
            | X =>
              Style.translate(
                ghost.dimensions.width +. ghost.margins.left +. ghost.margins.right,
                0.,
              )
            | Y =>
              Style.translate(
                0.,
                ghost.dimensions.height +. ghost.margins.top +. ghost.margins.bottom,
              )
            },
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>

      | Some(Alpha) =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~boxSizing="border-box",
            ~pointerEvents="none",
            ~userSelect="none",
            ~transition=Style.transition("transform"),
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>

      | Some(Omega) =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~boxSizing="border-box",
            ~pointerEvents="none",
            ~userSelect="none",
            ~transition=Style.transition("transform"),
            ~transform=switch ghost.axis {
            | X =>
              Style.translate(
                ghost.dimensions.width +. ghost.margins.left +. ghost.margins.right,
                0.,
              )
            | Y =>
              Style.translate(
                0.,
                ghost.dimensions.height +. ghost.margins.top +. ghost.margins.bottom,
              )
            },
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>

      | None =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(
            ~boxSizing="border-box",
            ~pointerEvents="none",
            ~userSelect="none",
            ~transition=Style.transition("transform"),
            (),
          )->ReactDOM.Style.unsafeAddProp("WebkitUserSelect", "none")}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>
      }

    | StandBy
    | Collecting(_) =>
      switch children {
      | #Children(_) =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          tabIndex=0
          style={ReactDOM.Style.make(~boxSizing="border-box", ())}
          className=?{className->Option.map(fn => fn(~dragging=false))}
          onMouseDown
          onTouchStart>
          children'
        </div>
      | #ChildrenWithDragHandle(_) =>
        <div
          ref={element->ReactDOM.Ref.domRef}
          style={ReactDOM.Style.make(~boxSizing="border-box", ())}
          className=?{className->Option.map(fn => fn(~dragging=false))}>
          children'
        </div>
      }
    }
  }
}
