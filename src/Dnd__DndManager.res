open Dnd__Types

module Context = Dnd__DndContext
module Events = Dnd__Events
module Style = Dnd__Style
module Geometry = Dnd__Geometry
module Scroller = Dnd__Scroller
module Scrollable = Dnd__Scrollable
module Web = Dnd__Web

module Make = (Context: Context.T) => {
  module Item = Context.Item
  module Container = Context.Container

  module ComparableItem = Belt.Id.MakeComparable({
    type t = Item.t
    let cmp = Item.cmp
  })

  module ComparableContainer = Belt.Id.MakeComparable({
    type t = Container.t
    let cmp = Container.cmp
  })

  type state = {
    status: Status.t<Item.t, Container.t>,
    prevStatus: Status.t<Item.t, Container.t>,
  }

  type action =
    | CollectEntries(
        Item.t,
        Container.t,
        RelativityBag.t<Point.t>,
        RelativityBag.t<Point.t>,
        [#Mouse | #Touch],
      )
    | StartDragging(Ghost.t<Item.t, Container.t>, Subscriptions.t)
    | UpdateGhostPosition(Ghost.t<Item.t, Container.t>)
    | StartDropping
    | CancelDrag
    | Reset

  module Layout = {
    let calculateDeltaToLandGhostBeforeItem = (
      ~axis: Axis.t,
      ~scroll: Scroll.t,
      ~scrollableDelta: Delta.t,
      ~itemRect: RelativityBag.t<Rect.t>,
      ~itemGeometry: Dnd__Types.Geometry.t,
      ~ghostMargins: Margins.t,
      ~ghostDepartureRect: RelativityBag.t<Rect.t>,
      ~ghostDeparturePoint: RelativityBag.t<Point.t>,
    ) =>
      switch axis {
      | X =>
        open Delta
        {
          x: // From the left bound of the right sibling
          itemRect.page.left -.
          (// substract its left and ghost's right margins
          itemGeometry.margins.left +. ghostMargins.right) -.
          (// substract distance between ghost's right bound and the pointer
          ghostDepartureRect.page.right -. ghostDeparturePoint.page.x) -.
          // finally, substract distance from the left side of the page
          ghostDeparturePoint.page.x,
          y: itemRect.page.top -.
          ghostDepartureRect.page.top -.
          (scroll.delta.y +.
          scrollableDelta.y),
        }
      | Y =>
        open Delta
        {
          x: itemRect.page.left -.
          ghostDepartureRect.page.left -.
          (scroll.delta.x +.
          scrollableDelta.x),
          y: // From the top bound of the sibling below
          itemRect.page.top -.
          (// substract its top and ghost's bottom margins
          itemGeometry.margins.top +. ghostMargins.bottom) -.
          (// substract distance between ghost's bottom bound and the pointer
          ghostDepartureRect.page.bottom -. ghostDeparturePoint.page.y) -.
          // finally, substract distance from the top of the page
          ghostDeparturePoint.page.y,
        }
      }

    let calculateDeltaToLandGhostAfterItem = (
      ~axis: Axis.t,
      ~scroll: Scroll.t,
      ~scrollableDelta: Delta.t,
      ~itemRect: RelativityBag.t<Rect.t>,
      ~itemGeometry: Dnd__Types.Geometry.t,
      ~ghostMargins: Margins.t,
      ~ghostDepartureRect: RelativityBag.t<Rect.t>,
      ~ghostDeparturePoint: RelativityBag.t<Point.t>,
    ) =>
      switch axis {
      | X =>
        open Delta
        {
          x: // To the right bound of the left sibling
          itemRect.page.right +.
          (// add its right and ghost's left margins
          itemGeometry.margins.right +. ghostMargins.left) +.
          (// add distance between the pointer and ghost's left bound
          ghostDeparturePoint.page.x -. ghostDepartureRect.page.left) -.
          // finally, substract distance from the left side of the page
          ghostDeparturePoint.page.x,
          y: itemRect.page.top -.
          ghostDepartureRect.page.top -.
          (scroll.delta.y +.
          scrollableDelta.y),
        }
      | Y =>
        open Delta
        {
          x: itemRect.page.left -.
          ghostDepartureRect.page.left -.
          (scroll.delta.x +.
          scrollableDelta.x),
          y: // To the bottom bound of the sibling above
          itemRect.page.bottom +.
          (// add its bottom and ghost's top margins
          itemGeometry.margins.bottom +. ghostMargins.top) +.
          (// add distance between the pointer and ghost's top bound
          ghostDeparturePoint.page.y -. ghostDepartureRect.page.top) -.
          // finally, substract distance from the top of the page
          ghostDeparturePoint.page.y,
        }
      }

    let calculateDeltaToLandGhostOnEmptyContainer = (
      ~axis: Axis.t,
      ~containerGeometry: Dnd__Types.Geometry.t,
      ~ghostDimensions: Dimensions.t,
      ~ghostDepartureRect: RelativityBag.t<Rect.t>,
      ~ghostDeparturePoint: RelativityBag.t<Point.t>,
    ) =>
      switch axis {
      | X =>
        open Delta
        {
          x: // From the right bound of the container
          containerGeometry.rect.page.right -.
          (// substract the right border and the padding of the container
          containerGeometry.borders.right +. containerGeometry.paddings.right) -.
          (// substract distance between ghost's right bound and the pointer
          ghostDepartureRect.page.right -. ghostDeparturePoint.page.x) +.
          // add ghost's width since ghost pushed the right side of the container to the left
          ghostDimensions.width -.
          // finally, substract distance from the left side of the page
          ghostDeparturePoint.page.x,
          y: // To the top bound of the container
          containerGeometry.rect.page.top +.
          (// add the top border and padding of the container
          containerGeometry.borders.top +. containerGeometry.paddings.top) +.
          (// add distance between ghost's top bound and the pointer
          ghostDeparturePoint.page.y -. ghostDepartureRect.page.top) -.
          // finally, substract distance from the top of the page
          ghostDeparturePoint.page.y,
        }
      | Y => {
          x: // To the left bound of the container
          containerGeometry.rect.page.left +.
          (// add the left border and padding of the container
          containerGeometry.borders.left +. containerGeometry.paddings.left) +.
          (// add distance between ghost's left bound and the pointer
          ghostDeparturePoint.page.x -. ghostDepartureRect.page.left) -.
          // finally, substract distance from the left side of the page
          ghostDeparturePoint.page.x,
          y: // From the bottom bound of the container
          containerGeometry.rect.page.bottom -.
          (// substract the bottom border and padding of the container
          containerGeometry.borders.bottom +. containerGeometry.paddings.bottom) -.
          (// substract distance between ghost's bottom bound and the pointer
          ghostDepartureRect.page.bottom -. ghostDeparturePoint.page.y) +.
          // add ghost's height since ghost pushed the bottom side down
          ghostDimensions.height -.
          // finally, substract distance from the top of the page
          ghostDeparturePoint.page.y,
        }
      }
  }

  module MouseSubscriptions = {
    let onMouseMove = (updatePosition: React.ref<RelativityBag.t<Point.t> => unit>, event) => {
      {
        open // [%log.debug "MouseMove"; ("", "")];
        Webapi.Dom
        event->MouseEvent.preventDefault
      }

      let point = {
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

      updatePosition.current(point)
    }

    let onMouseUp = (startDropping: React.ref<unit => unit>, _event) => {
      %log.debug(
        "MouseUp"
        ("", "")
      )
      startDropping.current()
    }

    let onKeyDown = (cancelDrag: React.ref<unit => unit>, event) => {
      %log.debug(
        "KeyDown"
        ("", "")
      )
      switch event->Events.Keyboard.Dom.key {
      | Esc =>
        // Stopping propagation to prevent closing modal while dragging
        event->Webapi.Dom.KeyboardEvent.stopPropagation
        cancelDrag.current()
      | Tab
      | Space
      | Enter
      | ArrowUp
      | ArrowDown
      | ArrowLeft
      | ArrowRight
      | Other => ()
      }
    }

    let onResize = (cancelDrag: React.ref<unit => unit>, _event) => cancelDrag.current()

    let onVisibilityChange = (cancelDrag: React.ref<unit => unit>, _event) => cancelDrag.current()
  }

  module TouchSubscriptions = {
    let onTouchMove = (updatePosition: React.ref<RelativityBag.t<Point.t> => unit>, event) => {
      let event = event->Events.Touch.castEventToTouchEvent
      let touch = {
        open Webapi.Dom
        event->TouchEvent.touches->Events.Touch.castDomTouchListToTouchArray->Array.getUnsafe(0)
      }

      // NOTE: Chrome doesn't allow preventDefault anymore: https://www.chromestatus.com/features/5093566007214080
      // event->Webapi.Dom.TouchEvent.preventDefault;

      let point = {
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

      updatePosition.current(point)
    }

    let onTouchEnd = (startDropping: React.ref<unit => unit>, event) => {
      event->Webapi.Dom.Event.preventDefault
      startDropping.current()
    }

    let onContextMenu = event => event->Webapi.Dom.Event.preventDefault

    let onOrientationChange = (cancelDrag: React.ref<unit => unit>, _event) => cancelDrag.current()

    let onVisibilityChange = (cancelDrag: React.ref<unit => unit>, _event) => cancelDrag.current()
  }

  module Hook = {
    type t = (~itemId: Item.t) => unit

    let invoke = (fn: option<t>, ~itemId) =>
      switch fn {
      | Some(fn) => fn(~itemId)
      | None => ()
      }
  }

  @react.component
  let make = (
    ~onDragStart: option<Hook.t>=?,
    ~onDropStart: option<Hook.t>=?,
    ~onDropEnd: option<Hook.t>=?,
    ~onReorder: option<ReorderResult.t<Item.t, Container.t>> => unit,
    ~children,
  ) => {
    let items: React.ref<
      Map.t<Item.t, ItemBag.t<Item.t, Container.t>, ComparableItem.identity>,
    > = React.useRef(Map.make(~id=module(ComparableItem)))

    let containers: React.ref<
      Map.t<Container.t, ContainerBag.t<Item.t, Container.t>, ComparableContainer.identity>,
    > = React.useRef(Map.make(~id=module(ComparableContainer)))

    let scroll: React.ref<option<Scroll.t>> = React.useRef(None)
    let viewport: React.ref<option<Dimensions.t>> = React.useRef(None)

    let focusTargetToRestore: React.ref<option<Dom.htmlElement>> = React.useRef(None)

    let scheduledWindowScrollFrameId: React.ref<option<Webapi.rafId>> = React.useRef(None)
    let scheduledScrollableElementScrollFrameId: React.ref<option<Webapi.rafId>> = React.useRef(
      None,
    )

    let updateGhostPosition = React.useRef(_point =>
      %log.warn(
        "UpdateGhostPositionNotSet"
        ("Point", _point)
      )
    )
    let updateScrollPosition = React.useRef(_ghost =>
      %log.warn(
        "UpdateScrollPositionNotSet"
        ("Ghost", _ghost)
      )
    )
    let invalidateLayout = React.useRef(_ghost =>
      %log.warn(
        "InvalidateLayoutNotSet"
        ("Ghost", _ghost)
      )
    )
    let startDropping = React.useRef(() =>
      %log.warn(
        "StartDroppingNotSet"
        ("", "")
      )
    )
    let cancelDrag = React.useRef(() =>
      %log.warn(
        "CancelDragNotSet"
        ("", "")
      )
    )

    let (state, dispatch) = React.useReducer((state, action) =>
      switch action {
      | CollectEntries(itemId, containerId, startPoint, currentPoint, interaction) => {
          status: Collecting(itemId, containerId, startPoint, currentPoint, interaction),
          prevStatus: state.status,
        }
      | StartDragging(ghost, subscriptions) => {
          status: Dragging(ghost, subscriptions),
          prevStatus: state.status,
        }
      | UpdateGhostPosition(nextGhost) =>
        switch state.status {
        | Dragging(_, subscriptions) => {
            status: Dragging(nextGhost, subscriptions),
            prevStatus: state.status,
          }
        | Collecting(_)
        | Dropping(_)
        | StandBy => state
        }
      | StartDropping =>
        switch state.status {
        | Dragging(ghost, _) =>
          let (ghost, result) = switch ghost.targetContainer {
          | None =>
            %log.debug(
              "StartDropping::NoTargetContainer"
              ("", "")
            )
            let container = containers.current->Map.getExn(ghost.originalContainer)
            let scrollableDelta = switch container.scrollable {
            | Some(scrollable) =>
              open Delta
              {
                x: scrollable.scroll.delta.x,
                y: scrollable.scroll.delta.y,
              }
            | None =>
              open Delta
              {x: 0., y: 0.}
            }
            let nextGhost = {
              ...ghost,
              delta: switch ghost.axis {
              | X => {x: 0. -. scrollableDelta.x, y: 0.}
              | Y => {x: 0., y: 0. -. scrollableDelta.y}
              },
            }
            (nextGhost, None)

          | Some(targetContainerId) if ghost.targetingOriginalContainer =>
            %log.debug(
              "StartDropping::TargetingOriginalContainer"
              ("ContainerId", targetContainerId)
            )
            let container = containers.current->Map.getExn(targetContainerId)
            let scroll = scroll.current->Option.getExn
            let scrollableDelta = switch container.scrollable {
            | Some(scrollable) =>
              open Delta
              {
                x: scrollable.scroll.delta.x,
                y: scrollable.scroll.delta.y,
              }
            | None =>
              open Delta
              {x: 0., y: 0.}
            }

            let items =
              items.current
              ->Map.keep((_, item) =>
                item.containerId->Container.eq(targetContainerId) &&
                  !(item.id->Item.eq(ghost.itemId))
              )
              ->Map.reduce([], (acc, _, item) => {
                acc->Js.Array.push(item, _)->ignore
                acc
              })
              ->SortArray.stableSortBy((i1, i2) => compare(i1.originalIndex, i2.originalIndex))

            let before: option<
              [
                | #FirstOmegaItemFound(ItemBag.t<Item.t, Container.t>)
                | #NextItemAfterLastAlphaFound(ItemBag.t<Item.t, Container.t>)
                | #SearchingForLastAlphaItem(ItemBag.t<Item.t, Container.t>)
              ],
            > = items->Array.reduce(None, (res, item) =>
              switch (res, item.shift) {
              | (
                  Some(
                    #FirstOmegaItemFound(_)
                    | #NextItemAfterLastAlphaFound(_),
                  ),
                  _,
                ) => res
              | (None, Some(Omega)) => #FirstOmegaItemFound(item)->Some
              | (None, Some(Alpha)) => #SearchingForLastAlphaItem(item)->Some
              | (Some(#SearchingForLastAlphaItem(_)), Some(Alpha)) =>
                #SearchingForLastAlphaItem(item)->Some
              | (Some(#SearchingForLastAlphaItem(_)), None) =>
                #NextItemAfterLastAlphaFound(item)->Some
              | (None, None)
              | (Some(#SearchingForLastAlphaItem(_)), Some(Omega)) => res
              }
            )

            switch before {
            | Some(#FirstOmegaItemFound(item))
            | Some(#NextItemAfterLastAlphaFound(item)) =>
              %log.debug(
                "StartDropping::TargetingOriginalContainer::Result::Before"
                ("Item", item)
              )
              let itemGeometry = item.geometry->Option.getExn
              let itemRect = Geometry.shiftInternalSibling(
                ghost.axis,
                ghost.dimensions,
                itemGeometry,
                scroll,
                container.scrollable,
                item.shift,
              )
              (
                {
                  ...ghost,
                  delta: Layout.calculateDeltaToLandGhostBeforeItem(
                    ~axis=ghost.axis,
                    ~scroll,
                    ~scrollableDelta,
                    ~itemRect,
                    ~itemGeometry,
                    ~ghostMargins=ghost.margins,
                    ~ghostDepartureRect=ghost.departureRect,
                    ~ghostDeparturePoint=ghost.departurePoint,
                  ),
                },
                Some(ReorderResult.SameContainer(ghost.itemId, Before(item.id))),
              )

            | Some(#SearchingForLastAlphaItem(item)) =>
              %log.debug(
                "StartDropping::TargetingOriginalContainer::Result::Last"
                ("Item", item)
              )
              let itemGeometry = item.geometry->Option.getExn
              let itemRect = Geometry.shiftInternalSibling(
                ghost.axis,
                ghost.dimensions,
                itemGeometry,
                scroll,
                container.scrollable,
                item.shift,
              )
              (
                {
                  ...ghost,
                  delta: Layout.calculateDeltaToLandGhostAfterItem(
                    ~axis=ghost.axis,
                    ~scroll,
                    ~scrollableDelta,
                    ~itemRect,
                    ~itemGeometry,
                    ~ghostMargins=ghost.margins,
                    ~ghostDepartureRect=ghost.departureRect,
                    ~ghostDeparturePoint=ghost.departurePoint,
                  ),
                },
                Some(ReorderResult.SameContainer(ghost.itemId, Last)),
              )

            | None =>
              %log.debug(
                "StartDropping::TargetingOriginalContainer::Result::None"
                ("", "")
              )
              (
                {
                  ...ghost,
                  delta: {
                    x: 0. -. scrollableDelta.x,
                    y: 0. -. scrollableDelta.y,
                  },
                },
                None,
              )
            }

          | Some(targetContainerId) =>
            let container = containers.current->Map.getExn(targetContainerId)
            let scroll = scroll.current->Option.getExn
            let scrollableDelta = switch container.scrollable {
            | Some(scrollable) =>
              open Delta
              {
                x: scrollable.scroll.delta.x,
                y: scrollable.scroll.delta.y,
              }
            | None =>
              open Delta
              {x: 0., y: 0.}
            }

            let items =
              items.current
              ->Map.keep((_, item) =>
                item.containerId->Container.eq(targetContainerId) &&
                  !(item.id->Item.eq(ghost.itemId))
              )
              ->Map.reduce([], (acc, _, item) => {
                acc->Js.Array.push(item, _)->ignore
                acc
              })
              ->SortArray.stableSortBy((i1, i2) => compare(i1.originalIndex, i2.originalIndex))

            let before: option<ItemBag.t<Item.t, Container.t>> = items->Array.reduce(None, (
              res,
              item,
            ) =>
              switch (res, item.shift) {
              | (Some(_), _) => res
              | (None, Some(Omega)) => item->Some
              | (None, Some(Alpha))
              | (None, None) => res
              }
            )

            switch before {
            | Some(item) =>
              %log.debug(
                "StartDropping::TargetingNewContainer::Result::Before"
                ("Item", item)
              )
              let itemGeometry = item.geometry->Option.getExn
              let itemRect = Geometry.shiftExternalSibling(
                ghost.axis,
                ghost.dimensions,
                itemGeometry,
                scroll,
                container.scrollable,
                item.shift,
              )
              (
                {
                  ...ghost,
                  delta: Layout.calculateDeltaToLandGhostBeforeItem(
                    ~axis=ghost.axis,
                    ~scroll,
                    ~scrollableDelta,
                    ~itemRect,
                    ~itemGeometry,
                    ~ghostMargins=ghost.margins,
                    ~ghostDepartureRect=ghost.departureRect,
                    ~ghostDeparturePoint=ghost.departurePoint,
                  ),
                },
                Some(ReorderResult.NewContainer(ghost.itemId, targetContainerId, Before(item.id))),
              )

            | None =>
              switch items->Array.get(items->Array.length - 1) {
              | Some(item) =>
                %log.debug(
                  "StartDropping::TargetingNewContainer::Result::Last"
                  ("Item", item)
                )
                let itemGeometry = item.geometry->Option.getExn
                let itemRect = Geometry.shiftExternalSibling(
                  ghost.axis,
                  ghost.dimensions,
                  itemGeometry,
                  scroll,
                  container.scrollable,
                  item.shift,
                )
                (
                  {
                    ...ghost,
                    delta: Layout.calculateDeltaToLandGhostAfterItem(
                      ~axis=ghost.axis,
                      ~scroll,
                      ~scrollableDelta,
                      ~itemRect,
                      ~itemGeometry,
                      ~ghostMargins=ghost.margins,
                      ~ghostDepartureRect=ghost.departureRect,
                      ~ghostDeparturePoint=ghost.departurePoint,
                    ),
                  },
                  Some(ReorderResult.NewContainer(ghost.itemId, targetContainerId, Last)),
                )
              | None =>
                %log.debug(
                  "StartDropping::TargetingNewContainer::Result::EmptyContainer"
                  ("", "")
                )
                (
                  {
                    ...ghost,
                    delta: Layout.calculateDeltaToLandGhostOnEmptyContainer(
                      ~axis=ghost.axis,
                      ~containerGeometry=container.geometry->Option.getExn,
                      ~ghostDimensions=ghost.dimensions,
                      ~ghostDepartureRect=ghost.departureRect,
                      ~ghostDeparturePoint=ghost.departurePoint,
                    ),
                  },
                  Some(ReorderResult.NewContainer(ghost.itemId, targetContainerId, Last)),
                )
              }
            }
          }
          {status: Dropping(ghost, result), prevStatus: state.status}

        | Collecting(_)
        | Dropping(_)
        | StandBy => state
        }

      | CancelDrag =>
        switch state.status {
        | Dragging(ghost, _) =>
          let container = containers.current->Map.getExn(ghost.originalContainer)

          let nextGhost = switch container.scrollable {
          | Some(scrollable) => {
              ...ghost,
              delta: {
                x: 0. -. scrollable.scroll.delta.x,
                y: 0. -. scrollable.scroll.delta.y,
              },
            }
          | None => {
              ...ghost,
              delta: {
                x: 0.,
                y: 0.,
              },
            }
          }

          {status: Dropping(nextGhost, None), prevStatus: state.status}

        | Collecting(_)
        | Dropping(_)
        | StandBy => state
        }
      | Reset => {status: StandBy, prevStatus: state.status}
      }
    , {status: StandBy, prevStatus: StandBy})

    let registerItem = React.useCallback1(
      (item: ItemBag.registrationPayload<Item.t, Container.t>) =>
        items.current =
          items.current->Map.set(
            item.id,
            {
              id: item.id,
              containerId: item.containerId,
              originalIndex: item.index,
              targetIndex: item.index,
              element: item.element,
              shift: None,
              geometry: None,
              animating: false,
              getGeometry: item.getGeometry,
            },
          ),
      [items],
    )

    let registerContainer = React.useCallback1(
      (container: ContainerBag.registrationPayload<Item.t, Container.t>) =>
        containers.current =
          containers.current->Map.set(
            container.id,
            {
              id: container.id,
              axis: container.axis,
              lockAxis: container.lockAxis,
              element: container.element,
              geometry: None,
              scrollable: None,
              accept: container.accept,
              getGeometryAndScrollable: container.getGeometryAndScrollable,
            },
          ),
      [containers],
    )

    let disposeItem = React.useCallback1(
      itemId => items.current = items.current->Map.remove(itemId),
      [items],
    )

    let disposeContainer = React.useCallback1(
      containerId => containers.current = containers.current->Map.remove(containerId),
      [containers],
    )

    let collectEntries = React.useCallback0((
      itemId: Item.t,
      containerId: Container.t,
      startPoint: RelativityBag.t<Point.t>,
      currentPoint: RelativityBag.t<Point.t>,
      interaction: [#Mouse | #Touch],
    ) => CollectEntries(itemId, containerId, startPoint, currentPoint, interaction)->dispatch)

    let prepareDrag = React.useCallback2(
      (
        itemId: Item.t,
        containerId: Container.t,
        startPoint: RelativityBag.t<Point.t>,
        currentPoint: RelativityBag.t<Point.t>,
        interaction: [#Mouse | #Touch],
      ) => {
        open Webapi.Dom

        let item = items.current->Map.getExn(itemId)
        let container = containers.current->Map.getExn(containerId)

        let maxScroll = Scrollable.Window.getMaxScroll()
        let scrollPosition = Scrollable.Window.getScrollPosition()

        let rect = item.element->HtmlElement.getBoundingClientRect
        let style = window->Window.getComputedStyle(item.element->Web.htmlElementToElement)

        let viewportRect = rect->Geometry.getViewportRect
        let pageRect = viewportRect->Geometry.getPageRectFromViewportRect(scrollPosition)
        let currentRect = {
          open RelativityBag
          {page: pageRect, viewport: viewportRect}
        }

        let ghost = {
          open Ghost
          {
            itemId: itemId,
            originalContainer: containerId,
            targetContainer: containerId->Some,
            targetingOriginalContainer: true,
            axis: container.axis,
            lockAxis: container.lockAxis,
            element: item.element,
            direction: Geometry.getDirection(~was=startPoint.page.y, ~is=currentPoint.page.y),
            dimensions: rect->Geometry.getDimensions,
            margins: style->Geometry.getMargins,
            borders: style->Geometry.getBorders,
            departurePoint: currentPoint,
            currentPoint: currentPoint,
            departureRect: currentRect,
            currentRect: currentRect,
            delta: {
              x: 0.,
              y: 0.,
            },
          }
        }

        let subscriptions = switch interaction {
        | #Mouse =>
          let onMouseMove = MouseSubscriptions.onMouseMove(updateGhostPosition)
          let onMouseUp = MouseSubscriptions.onMouseUp(startDropping)
          let onKeyDown = MouseSubscriptions.onKeyDown(cancelDrag)
          let onResize = MouseSubscriptions.onResize(cancelDrag)
          let onVisibilityChange = MouseSubscriptions.onVisibilityChange(cancelDrag)

          open Subscriptions
          {
            install: () => {
              %log.debug(
                "MouseSubscriptions::SubscriptionsInstalled"
                ("ItemId", itemId)
              )
              Window.addMouseMoveEventListener(window, onMouseMove)
              Window.addMouseUpEventListener(window, onMouseUp)
              HtmlElement.addKeyDownEventListener(ghost.element, onKeyDown)
              Window.addEventListener(window, "resize", onResize)
              Window.addEventListener(window, "visibilitychange", onVisibilityChange)
            },
            drop: () => {
              %log.debug(
                "MouseSubscriptions::SubscriptionsDropped"
                ("ItemId", itemId)
              )
              Window.removeMouseMoveEventListener(window, onMouseMove)
              Window.removeMouseUpEventListener(window, onMouseUp)
              HtmlElement.removeKeyDownEventListener(ghost.element, onKeyDown)
              Window.removeEventListener(window, "resize", onResize)
              Window.removeEventListener(window, "visibilitychange", onVisibilityChange)
            },
          }
        | #Touch =>
          let onTouchMove = TouchSubscriptions.onTouchMove(updateGhostPosition)
          let onTouchEnd = TouchSubscriptions.onTouchEnd(startDropping)
          let onContextMenu = TouchSubscriptions.onContextMenu
          let onOrientationChange = TouchSubscriptions.onOrientationChange(cancelDrag)
          let onVisibilityChange = TouchSubscriptions.onVisibilityChange(cancelDrag)

          open Subscriptions
          {
            install: () => {
              %log.debug(
                "TouchSubscriptions::SubscriptionsInstalled"
                ("ItemId", itemId)
              )
              Window.addEventListener(window, "touchmove", onTouchMove)
              Window.addEventListener(window, "touchend", onTouchEnd)
              Window.addEventListener(window, "contextmenu", onContextMenu)
              Window.addEventListener(window, "orientationchange", onOrientationChange)
              Window.addEventListener(window, "visibilitychange", onVisibilityChange)
            },
            drop: () => {
              %log.debug(
                "TouchSubscriptions::SubscriptionsDropped"
                ("ItemId", itemId)
              )
              Window.removeEventListener(window, "touchmove", onTouchMove)
              Window.removeEventListener(window, "touchend", onTouchEnd)
              Window.removeEventListener(window, "contextmenu", onContextMenu)
              Window.removeEventListener(window, "orientationchange", onOrientationChange)
              Window.removeEventListener(window, "visibilitychange", onVisibilityChange)
            },
          }
        }

        items.current =
          items.current->Map.map(item => {...item, geometry: item.getGeometry()->Some})

        containers.current =
          containers.current->Map.map(container => {
            let (geometry, scrollable) = container.getGeometryAndScrollable()
            {...container, geometry: geometry->Some, scrollable: scrollable}
          })

        viewport.current = Geometry.getViewport()->Some

        scroll.current = Some({
          open Scroll
          {
            max: maxScroll,
            initial: scrollPosition,
            current: scrollPosition,
            delta: {
              x: 0.,
              y: 0.,
            },
          }
        })

        // Saving currently focused element and focusing ghost element
        // to be able to listen keyboard events to stop event propagation.
        // This is useful when drag is happening inside a modal
        // that can be closed via Esc key.
        // We don't want that in the middle of dragging.
        focusTargetToRestore.current =
          document
          ->Document.unsafeAsHtmlDocument
          ->HtmlDocument.activeElement
          ->Option.map(Element.unsafeAsHtmlElement)

        ghost.element->Webapi.Dom.HtmlElement.focus

        StartDragging(ghost, subscriptions)->dispatch
      },
      (items, containers),
    )

    let resetAnimationsOnDrag = React.useCallback1(itemIds =>
      switch itemIds {
      | list{} => ()
      | _ as ids =>
        items.current =
          ids->List.reduceU(items.current, (. map, id) =>
            map->Map.updateU(id, (. item) =>
              switch item {
              | Some(item) =>
                Some({
                  open ItemBag
                  {...item, animating: false}
                })
              | None => None
              }
            )
          )
      }
    , [items])

    React.useEffect2(() =>
      switch (state.prevStatus, state.status) {
      | (StandBy, Collecting(itemId, containerId, startPoint, currentPoint, interaction)) =>
        prepareDrag(itemId, containerId, startPoint, currentPoint, interaction)
        None
      | (Collecting(_), Dragging(ghost, subscriptions)) =>
        subscriptions.install()
        onDragStart->Hook.invoke(~itemId=ghost.itemId)
        None
      | (Dragging(_, _), Dropping(ghost, result)) =>
        onDropStart->Hook.invoke(~itemId=ghost.itemId)
        Js.Global.setTimeout(
          () => {
            result->onReorder
            Reset->dispatch
          },
          {
            open Style
            animationDuration + finishDropFactor
          },
        )->ignore
        None
      | (Dropping(ghost, _), StandBy) =>
        focusTargetToRestore.current->Option.map(Webapi.Dom.HtmlElement.focus)->ignore
        focusTargetToRestore.current = None

        items.current = Map.make(~id=module(ComparableItem))
        containers.current = Map.make(~id=module(ComparableContainer))
        scroll.current = None
        viewport.current = None
        onDropEnd->Hook.invoke(~itemId=ghost.itemId)
        None
      | (
          StandBy | Collecting(_) | Dragging(_) | Dropping(_),
          StandBy | Collecting(_) | Dragging(_) | Dropping(_),
        ) =>
        None
      }
    , (state.status, state.prevStatus))

    updateGhostPosition.current = React.useCallback3((nextPoint: RelativityBag.t<Point.t>) =>
      switch state.status {
      | Dragging(ghost, _) =>
        let nextPoint = if !ghost.lockAxis {
          nextPoint
        } else {
          switch ghost.axis {
          | X => {
              page: {
                ...nextPoint.page,
                y: ghost.departurePoint.page.y,
              },
              viewport: {
                ...nextPoint.viewport,
                y: ghost.departurePoint.viewport.y,
              },
            }
          | Y => {
              page: {
                ...nextPoint.page,
                x: ghost.departurePoint.page.x,
              },
              viewport: {
                ...nextPoint.viewport,
                x: ghost.departurePoint.viewport.x,
              },
            }
          }
        }

        let targetContainer =
          containers.current
          ->Map.valuesToArray
          ->Js.Array.find((container: ContainerBag.t<Item.t, Container.t>) => {
            let geometry = container.geometry->Option.getExn
            let rect = switch container.scrollable {
            | None => geometry.rect.page
            | Some(scrollable) =>
              open Rect
              {
                top: scrollable.geometry.rect.page.top > geometry.rect.page.top
                  ? scrollable.geometry.rect.page.top
                  : geometry.rect.page.top,
                bottom: scrollable.geometry.rect.page.bottom < geometry.rect.page.bottom
                  ? scrollable.geometry.rect.page.bottom
                  : geometry.rect.page.bottom,
                left: scrollable.geometry.rect.page.left > geometry.rect.page.left
                  ? scrollable.geometry.rect.page.left
                  : geometry.rect.page.left,
                right: scrollable.geometry.rect.page.right < geometry.rect.page.right
                  ? scrollable.geometry.rect.page.right
                  : geometry.rect.page.right,
              }
            }

            open ContainerBag
            container.accept
            ->Option.map(accept => ghost.itemId->accept)
            ->Option.getWithDefault(true) && {
                open Geometry
                nextPoint.page->isWithin(rect)
              }
          }, _)
          ->Option.map(container => container.id)

        let targetingOriginalContainer = switch targetContainer {
        | None => true
        | Some(targetContainer) if targetContainer->Container.eq(ghost.originalContainer) => true
        | Some(_) => false
        }

        let nextRect = {
          open RelativityBag
          {
            page: {
              open Rect
              {
                top: ghost.currentRect.page.top +. nextPoint.page.y -. ghost.currentPoint.page.y,
                bottom: ghost.currentRect.page.bottom +.
                nextPoint.page.y -.
                ghost.currentPoint.page.y,
                left: ghost.currentRect.page.left +. nextPoint.page.x -. ghost.currentPoint.page.x,
                right: ghost.currentRect.page.right +.
                nextPoint.page.x -.
                ghost.currentPoint.page.x,
              }
            },
            viewport: {
              open Rect
              {
                top: ghost.currentRect.viewport.top +.
                nextPoint.viewport.y -.
                ghost.currentPoint.viewport.y,
                bottom: ghost.currentRect.viewport.bottom +.
                nextPoint.viewport.y -.
                ghost.currentPoint.viewport.y,
                left: ghost.currentRect.viewport.left +.
                nextPoint.viewport.x -.
                ghost.currentPoint.viewport.x,
                right: ghost.currentRect.viewport.right +.
                nextPoint.viewport.x -.
                ghost.currentPoint.viewport.x,
              }
            },
          }
        }

        let nextDirection = switch ghost.axis {
        | X => Geometry.getDirection(~was=ghost.currentPoint.viewport.x, ~is=nextPoint.viewport.x)
        | Y => Geometry.getDirection(~was=ghost.currentPoint.viewport.y, ~is=nextPoint.viewport.y)
        }

        let nextGhost = {
          ...ghost,
          targetContainer: targetContainer,
          targetingOriginalContainer: targetingOriginalContainer,
          direction: switch nextDirection {
          | Some(direction) => Some(direction)
          | None => ghost.direction
          },
          currentRect: nextRect,
          currentPoint: nextPoint,
          delta: {
            x: nextPoint.page.x -. ghost.departurePoint.page.x,
            y: nextPoint.page.y -. ghost.departurePoint.page.y,
          },
        }

        invalidateLayout.current(nextGhost)
      | Collecting(_)
      | Dropping(_)
      | StandBy => ()
      }
    , (state.status, items, containers))

    updateScrollPosition.current = React.useCallback2((ghost: Ghost.t<Item.t, Container.t>) => {
      let scrollable = containers.current->Map.reduce(None, (
        scrollable: option<ScrollableElement.t>,
        _,
        container,
      ) =>
        switch (scrollable, container.scrollable) {
        | (Some(scrollable), Some(scrollable')) =>
          if (
            Geometry.contains(
              ~parent=scrollable'.geometry.rect.page,
              ~child=scrollable.geometry.rect.page,
            )
          ) {
            Some(scrollable)
          } else {
            Some(scrollable')
          }
        | (None, Some(scrollable')) =>
          if ghost.currentPoint.page->Geometry.isWithin(scrollable'.geometry.rect.page) {
            Some(scrollable')
          } else {
            None
          }
        | (Some(scrollable), None) => Some(scrollable)
        | (None, None) => None
        }
      )

      let scroller = Scroller.getScroller(
        ~point=ghost.currentPoint,
        ~viewport=viewport.current->Option.getExn,
        ~scroll=scroll.current->Option.getExn,
        ~scrollable,
      )

      switch scroller {
      | Some(Window(requestWindowScroll)) =>
        switch scheduledWindowScrollFrameId.current {
        | Some(frameId) =>
          frameId->Webapi.cancelAnimationFrame
          scheduledWindowScrollFrameId.current = None
        | None => ()
        }

        scheduledWindowScrollFrameId.current = requestWindowScroll(() => {
          let currentScroll = scroll.current->Option.getExn

          let nextScrollPosition = Scrollable.Window.getScrollPosition()

          let nextScroll = {
            open Scroll
            {
              ...currentScroll,
              current: nextScrollPosition,
              delta: {
                x: nextScrollPosition.x -. currentScroll.initial.x,
                y: nextScrollPosition.y -. currentScroll.initial.y,
              },
            }
          }

          let nextPoint = {
            open RelativityBag
            {
              page: {
                open Point
                {
                  x: ghost.currentPoint.page.x +. nextScroll.current.x -. currentScroll.current.x,
                  y: ghost.currentPoint.page.y +. nextScroll.current.y -. currentScroll.current.y,
                }
              },
              viewport: {
                open Point
                {
                  x: ghost.currentPoint.viewport.x,
                  y: ghost.currentPoint.viewport.y,
                }
              },
            }
          }

          let delta = {
            open Delta
            {
              x: nextScrollPosition.x -. currentScroll.current.x,
              y: nextScrollPosition.y -. currentScroll.current.y,
            }
          }

          containers.current =
            containers.current->Map.map(container =>
              switch container.scrollable {
              | Some(scrollable) => {
                  ...container,
                  geometry: container.geometry->Option.map(geometry => {
                    ...geometry,
                    rect: geometry.rect->Geometry.shiftViewportRect(delta),
                  }),
                  scrollable: Some({
                    ...scrollable,
                    geometry: {
                      ...scrollable.geometry,
                      rect: scrollable.geometry.rect->Geometry.shiftViewportRect(delta),
                    },
                  }),
                }
              | None => {
                  ...container,
                  geometry: container.geometry->Option.map(geometry => {
                    ...geometry,
                    rect: geometry.rect->Geometry.shiftViewportRect(delta),
                  }),
                }
              }
            )

          scroll.current = nextScroll->Some

          updateGhostPosition.current(nextPoint)
        })

      | Some(Element(requestElementScroll)) =>
        switch scheduledScrollableElementScrollFrameId.current {
        | Some(frameId) =>
          frameId->Webapi.cancelAnimationFrame
          scheduledScrollableElementScrollFrameId.current = None
        | None => ()
        }

        scheduledScrollableElementScrollFrameId.current = requestElementScroll(scrollable => {
          let nextScrollPosition = scrollable.element->Scrollable.Element.getScrollPosition

          let nextScroll = {
            open Scroll
            {
              ...scrollable.scroll,
              current: nextScrollPosition,
              delta: {
                open Delta
                {
                  x: nextScrollPosition.x -. scrollable.scroll.initial.x,
                  y: nextScrollPosition.y -. scrollable.scroll.initial.y,
                }
              },
            }
          }

          let delta = {
            open Delta
            {
              x: nextScrollPosition.x -. scrollable.scroll.current.x,
              y: nextScrollPosition.y -. scrollable.scroll.current.y,
            }
          }

          containers.current =
            containers.current->Map.map(container =>
              switch container.scrollable {
              | Some(scrollable') if scrollable'.element === scrollable.element => {
                  ...container,
                  geometry: container.geometry->Option.map(geometry => {
                    ...geometry,
                    rect: geometry.rect->Geometry.shiftRects(delta),
                  }),
                  scrollable: Some({...scrollable, scroll: nextScroll}),
                }
              | Some(scrollable')
                if Geometry.contains(
                  ~parent=scrollable.geometry.rect.page,
                  ~child=scrollable'.geometry.rect.page,
                ) => {
                  ...container,
                  geometry: container.geometry->Option.map(geometry => {
                    ...geometry,
                    rect: geometry.rect->Geometry.shiftRects(delta),
                  }),
                }
              | Some(_)
              | None => container
              }
            )

          invalidateLayout.current(ghost)
        })

      | None => ()
      }
    }, (items, containers))

    invalidateLayout.current = React.useCallback2((ghost: Ghost.t<Item.t, Container.t>) => {
      let (nextItems, animate) = switch ghost.targetContainer {
      | None =>
        let items = items.current->Map.map(item => {...item, shift: None})
        (items, list{})
      | Some(targetContainerId) =>
        let container = containers.current->Map.getExn(targetContainerId)

        items.current->Map.reduce((items.current, list{}), ((items, animate), id, item) =>
          switch item.containerId {
          | itemContainerId
            if targetContainerId->Container.eq(itemContainerId) &&
              ghost.targetingOriginalContainer =>
            let geometry = item.geometry->Option.getExn
            let scroll = scroll.current->Option.getExn

            let shiftedItemRect = Geometry.shiftInternalSibling(
              ghost.axis,
              ghost.dimensions,
              geometry,
              scroll,
              container.scrollable,
              item.shift,
            )
            let currentStatus = Geometry.isAforeAdjusted(
              ~subject=ghost.currentRect.page,
              ~comparand=shiftedItemRect.page,
              ~axis=ghost.axis,
              ~direction=ghost.direction,
            )
              ? #GhostIsBefore
              : #GhostIsAfter

            let initialStatus = Geometry.isAfore(
              ~subject=ghost.departureRect.page,
              ~comparand=geometry.rect.page,
              ~axis=ghost.axis,
            )
              ? #GhostWasBefore
              : #GhostWasAfter

            switch (item.shift, currentStatus, initialStatus) {
            // Dragging this one, no changes required
            | (_, _, _) if item.id->Item.eq(ghost.itemId) => (items, animate)

            // This one is in the middle of transition, ignoring
            | (_, _, _) if item.animating => (items, animate)

            // Ghost is before item but initially was after:
            // item is already Omega, so ignoring
            | (Some(Omega), #GhostIsBefore, #GhostWasAfter) => (items, animate)

            // Ghost is before item but initially was after:
            // item is not Omega yet, so pushing it
            | (_, #GhostIsBefore, #GhostWasAfter) => (
                items->Map.set(
                  id,
                  {
                    ...item,
                    shift: Omega->Some,
                    targetIndex: item.originalIndex + 1,
                    animating: true,
                  },
                ),
                list{id, ...animate},
              )

            // Ghost is after item but initially was before:
            // item is already Alpha, so ignoring
            | (Some(Alpha), #GhostIsAfter, #GhostWasBefore) => (items, animate)

            // Ghost is after item but initially was before:
            // item is not Alpha yet, so pushing it
            | (_, #GhostIsAfter, #GhostWasBefore) => (
                items->Map.set(
                  id,
                  {
                    ...item,
                    shift: Alpha->Some,
                    targetIndex: item.originalIndex - 1,
                    animating: true,
                  },
                ),
                list{id, ...animate},
              )

            // If we got here, item should go to its original position:
            // since it was shifted — pushing it back
            | (Some(Alpha | Omega), _, _) => (
                items->Map.set(
                  id,
                  {
                    ...item,
                    shift: None,
                    targetIndex: item.originalIndex,
                    animating: true,
                  },
                ),
                list{id, ...animate},
              )

            // Item is in its original position, no updates required
            | (None, _, _) => (items, animate)
            }

          | itemContainerId
            if targetContainerId->Container.eq(itemContainerId) &&
              !ghost.targetingOriginalContainer =>
            let geometry = item.geometry->Option.getExn
            let scroll = scroll.current->Option.getExn

            let shiftedDraggableRect = Geometry.shiftExternalSibling(
              ghost.axis,
              ghost.dimensions,
              geometry,
              scroll,
              container.scrollable,
              item.shift,
            )
            let currentStatus = Geometry.isAforeAdjusted(
              ~subject=ghost.currentRect.page,
              ~comparand=shiftedDraggableRect.page,
              ~axis=ghost.axis,
              ~direction=ghost.direction,
            )
              ? #GhostIsBefore
              : #GhostIsAfter
            switch (item.animating, item.shift, currentStatus) {
            // This one is in the middle of transition, ignoring
            | (true, _, _) => (items, animate)

            // Ghost is before item: item is already Omega, so ignoring
            | (false, Some(Omega), #GhostIsBefore) => (items, animate)

            // Ghost is before item: item is not Omega yet, so pushing it
            | (false, Some(Alpha) | None, #GhostIsBefore) => (
                items->Map.set(
                  id,
                  {
                    ...item,
                    shift: Omega->Some,
                    targetIndex: item.originalIndex + 1,
                    animating: true,
                  },
                ),
                list{id, ...animate},
              )

            // Ghost is after item: item is already Alpha, so ignoring
            | (false, Some(Alpha), #GhostIsAfter) => (items, animate)

            // Ghost is after item: item is not Alpha yet, so pushing it
            | (false, Some(Omega) | None, #GhostIsAfter) => (
                items->Map.set(
                  id,
                  {
                    ...item,
                    shift: Alpha->Some,
                    targetIndex: item.originalIndex - 1,
                    animating: true,
                  },
                ),
                list{id, ...animate},
              )
            }

          // If we got here, item should go to its original position
          | _ => (
              items->Map.set(
                id,
                {
                  ...item,
                  shift: None,
                  targetIndex: item.originalIndex,
                },
              ),
              animate,
            )
          }
        )
      }

      items.current = nextItems

      UpdateGhostPosition(ghost)->dispatch

      updateScrollPosition.current(ghost)

      Js.Global.setTimeout(
        () => animate->resetAnimationsOnDrag,
        {
          open Style
          animationDuration + resetAnimationsFactor
        },
      )->ignore
    }, (items, containers))

    let prepareDrop = React.useCallback3(() => {
      switch state.status {
      | Dragging(_, subscriptions) => subscriptions.drop()
      | Collecting(_)
      | Dropping(_)
      | StandBy => ()
      }
      switch scheduledWindowScrollFrameId.current {
      | Some(frameId) =>
        frameId->Webapi.cancelAnimationFrame
        scheduledWindowScrollFrameId.current = None
      | None => ()
      }

      switch scheduledScrollableElementScrollFrameId.current {
      | Some(frameId) =>
        frameId->Webapi.cancelAnimationFrame
        scheduledScrollableElementScrollFrameId.current = None
      | None => ()
      }
    }, (state.status, scheduledWindowScrollFrameId, scheduledScrollableElementScrollFrameId))

    startDropping.current = React.useCallback1(() => {
      prepareDrop()
      StartDropping->dispatch
    }, [prepareDrop])

    cancelDrag.current = React.useCallback2(() => {
      prepareDrop()
      items.current = items.current->Map.map(item => {...item, shift: None})
      CancelDrag->dispatch
    }, (items, prepareDrop))

    // HACK: We have to add persistent event listener due to webkit bug:
    //       https://bugs.webkit.org/show_bug.cgi?id=184250
    React.useEffect1(() => {
      %log.debug(
        "AddTouchMoveWebkitEventListener"
        ("Status", state.status)
      )
      let preventTouchMoveInWebkit = event => {
        open Webapi.Dom
        switch state.status {
        | Dragging(_) => event->Event.preventDefault
        | StandBy
        | Collecting(_)
        | Dropping(_) => ()
        }
      }

      preventTouchMoveInWebkit->Events.subscribeToTouchMove

      Some(
        () => {
          %log.debug(
            "RemoveTouchMoveWebkitEventListener"
            ("Status", state.status)
          )
          preventTouchMoveInWebkit->Events.unsubscribeFromTouchMove
        },
      )
    }, [state.status])

    <Context.Provider
      value={
        status: state.status,
        scroll: scroll.current,
        target: switch state.status {
        | Dragging(ghost, _)
        | Dropping(ghost, _) =>
          ghost.targetContainer
        | Collecting(_, containerId, _, _, _) => containerId->Some
        | StandBy => None
        },
        registerItem: registerItem,
        registerContainer: registerContainer,
        disposeItem: disposeItem,
        disposeContainer: disposeContainer,
        getItemShift: itemId => (items.current->Map.getExn(itemId)).shift,
        startDragging: collectEntries,
      }>
      children
    </Context.Provider>
  }
}
