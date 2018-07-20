open Dnd__Config;
open Dnd__Types;

module Html = Dnd__Html;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;
module Scroller = Dnd__Scroller;
module Scrollable = Dnd__Scrollable;

module Make = (Cfg: Config) => {
  module DraggableComparator =
    Id.MakeComparable({
      type t = Cfg.Draggable.t;
      let cmp = Pervasives.compare;
    });

  module DroppableComparator =
    Id.MakeComparable({
      type t = Cfg.Droppable.t;
      let cmp = Pervasives.compare;
    });

  type state = {
    status: Status.t(Cfg.Draggable.t, Cfg.Droppable.t),
    draggables:
      ref(
        Map.t(
          Cfg.Draggable.t,
          DraggableBag.t(Cfg.Draggable.t, Cfg.Droppable.t),
          DraggableComparator.identity,
        ),
      ),
    droppables:
      ref(
        Map.t(
          Cfg.Droppable.t,
          DroppableBag.t(Cfg.Draggable.t, Cfg.Droppable.t),
          DroppableComparator.identity,
        ),
      ),
    scroll: ref(option(Scroll.t)),
    viewport: ref(option(Dimensions.t)),
    scheduledWindowScrollFrameId: ref(option(Webapi.rafId)),
    scheduledScrollableElementScrollFrameId: ref(option(Webapi.rafId)),
    scheduledLayoutRecalculationFrameId: ref(option(Webapi.rafId)),
  };

  type action =
    | PrepareDrag(
        Cfg.Draggable.t,
        Cfg.Droppable.t,
        RelativityBag.t(Point.t),
        RelativityBag.t(Point.t),
        Dom.htmlElement,
        Subscriptions.t,
      )
    | StartDragging(
        Ghost.t(Cfg.Draggable.t, Cfg.Droppable.t),
        Subscriptions.t,
      )
    | UpdateGhostPosition(RelativityBag.t(Point.t))
    | UpdateScrollPosition
    | RecalculateLayout(Ghost.t(Cfg.Draggable.t, Cfg.Droppable.t))
    | ResetAnimations(list(Cfg.Draggable.t))
    | PrepareDrop
    | StartDropping
    | FinishDropping(DropResult.t(Cfg.Draggable.t, Cfg.Droppable.t))
    | CancelDrag
    | Reset;

  module Handlers = {
    let registerDraggable =
        (
          draggable:
            DraggableBag.registrationPayload(
              Cfg.Draggable.t,
              Cfg.Droppable.t,
            ),
          {ReasonReact.state},
        ) =>
      switch (state.status) {
      | StandBy =>
        state.draggables :=
          state.draggables^
          |. Map.set(
               draggable.id,
               {
                 id: draggable.id,
                 droppableId: draggable.droppableId,
                 geometry: None,
                 shift: None,
                 animating: false,
                 getGeometry: draggable.getGeometry,
               },
             )
      | Dragging(_, _)
      | Dropping(_) => ()
      };

    let registerDroppable =
        (
          droppable:
            DroppableBag.registrationPayload(
              Cfg.Draggable.t,
              Cfg.Droppable.t,
            ),
          {ReasonReact.state},
        ) =>
      switch (state.status) {
      | StandBy =>
        state.droppables :=
          state.droppables^
          |. Map.set(
               droppable.id,
               {
                 id: droppable.id,
                 axis: droppable.axis,
                 geometry: None,
                 scrollable: None,
                 accept: droppable.accept,
                 getGeometryAndScrollable: droppable.getGeometryAndScrollable,
               },
             )
      | Dragging(_, _)
      | Dropping(_) => ()
      };

    let disposeDraggable = (draggableId, {ReasonReact.state}) =>
      state.draggables := state.draggables^ |. Map.remove(draggableId);

    let disposeDroppable = (droppableId, {ReasonReact.state}) =>
      state.droppables := state.droppables^ |. Map.remove(droppableId);
  };

  let component = ReasonReact.reducerComponent("DndContext");

  let make = (~onDrop, children) => {
    ...component,
    initialState: () => {
      status: StandBy,
      draggables: ref(Map.make(~id=(module DraggableComparator))),
      droppables: ref(Map.make(~id=(module DroppableComparator))),
      scroll: ref(None),
      viewport: ref(None),
      scheduledWindowScrollFrameId: ref(None),
      scheduledScrollableElementScrollFrameId: ref(None),
      scheduledLayoutRecalculationFrameId: ref(None),
    },
    reducer: (action, state) =>
      switch (action) {
      | PrepareDrag(
          draggableId,
          droppableId,
          startPoint,
          currentPoint,
          element,
          subscriptions,
        ) =>
        ReasonReact.SideEffects(
          (
            ({state, send}) => {
              open Webapi.Dom;

              let droppable = state.droppables^ |. Map.getExn(droppableId);

              let maxScroll = Scrollable.Window.getMaxScroll();
              let scrollPosition = Scrollable.Window.getScrollPosition();

              let rect = element |. HtmlElement.getBoundingClientRect;
              let style =
                element
                |. Html.castHtmlElementToElement
                |. Window.getComputedStyle(window);

              let viewportRect = rect |> Geometry.getViewportRect;
              let pageRect =
                scrollPosition
                |> Geometry.getPageRectFromViewportRect(viewportRect);
              let currentRect =
                RelativityBag.{page: pageRect, viewport: viewportRect};

              let ghost =
                Ghost.{
                  element,
                  draggableId,
                  originalDroppable: droppableId,
                  targetDroppable: Some(droppableId),
                  targetingOriginalDroppable: true,
                  axis: droppable.axis,
                  direction:
                    Geometry.getDirection(
                      ~was=startPoint.page.y,
                      ~is=currentPoint.page.y,
                    ),
                  dimensions: rect |. Geometry.getDimensions,
                  margins: style |. Geometry.getMargins,
                  borders: style |. Geometry.getBorders,
                  departurePoint: currentPoint,
                  currentPoint,
                  departureRect: currentRect,
                  currentRect,
                  delta: {
                    x: 0,
                    y: 0,
                  },
                };

              state.draggables :=
                state.draggables^
                |. Map.map(draggable =>
                     {...draggable, geometry: Some(draggable.getGeometry())}
                   );

              state.droppables :=
                state.droppables^
                |. Map.map(droppable => {
                     let (geometry, scrollable) =
                       droppable.getGeometryAndScrollable();
                     {...droppable, geometry: Some(geometry), scrollable};
                   });

              state.viewport := Some(Geometry.getViewport());

              state.scroll :=
                Some(
                  Scroll.{
                    max: maxScroll,
                    initial: scrollPosition,
                    current: scrollPosition,
                    delta: {
                      x: 0,
                      y: 0,
                    },
                  },
                );

              StartDragging(ghost, subscriptions) |> send;
            }
          ),
        )

      | StartDragging(ghost, subscriptions) =>
        ReasonReact.UpdateWithSideEffects(
          {...state, status: Dragging(ghost, subscriptions)},
          (_ => subscriptions.install()),
        )

      | UpdateGhostPosition(nextPoint) =>
        switch (state.status) {
        | Dragging(ghost, _) =>
          ReasonReact.SideEffects(
            (
              ({state, send}) => {
                switch (state.scheduledLayoutRecalculationFrameId^) {
                | Some(frameId) =>
                  frameId |> Webapi.cancelAnimationFrame;
                  state.scheduledLayoutRecalculationFrameId := None;
                | None => ()
                };

                state.scheduledLayoutRecalculationFrameId :=
                  Some(
                    Webapi.requestCancellableAnimationFrame(_ => {
                      let targetDroppable =
                        state.droppables^
                        |> Map.valuesToArray
                        |> Js.Array.find(
                             (
                               droppable:
                                 DroppableBag.t(
                                   Cfg.Draggable.t,
                                   Cfg.Droppable.t,
                                 ),
                             ) => {
                             let geometry =
                               droppable.geometry |. Option.getExn;
                             let rect =
                               switch (droppable.scrollable) {
                               | None => geometry.rect.page
                               | Some(scrollable) =>
                                 Rect.{
                                   top:
                                     scrollable.geometry.rect.page.top
                                     > geometry.rect.page.top ?
                                       scrollable.geometry.rect.page.top :
                                       geometry.rect.page.top,
                                   bottom:
                                     scrollable.geometry.rect.page.bottom
                                     < geometry.rect.page.bottom ?
                                       scrollable.geometry.rect.page.bottom :
                                       geometry.rect.page.bottom,
                                   left:
                                     scrollable.geometry.rect.page.left
                                     > geometry.rect.page.left ?
                                       scrollable.geometry.rect.page.left :
                                       geometry.rect.page.left,
                                   right:
                                     scrollable.geometry.rect.page.right
                                     < geometry.rect.page.right ?
                                       scrollable.geometry.rect.page.right :
                                       geometry.rect.page.right,
                                 }
                               };

                             DroppableBag.(
                               droppable.accept
                               |. Option.map(accept =>
                                    ghost.draggableId |> accept
                                  )
                               |. Option.getWithDefault(true)
                               && Geometry.(nextPoint.page |. isWithin(rect))
                             );
                           })
                        |. Option.map(droppable => droppable.id);

                      let targetingOriginalDroppable =
                        switch (targetDroppable) {
                        | None => true
                        | Some(targetDroppable)
                            when
                              Cfg.Droppable.eq(
                                targetDroppable,
                                ghost.originalDroppable,
                              ) =>
                          true
                        | Some(_) => false
                        };

                      let nextRect =
                        RelativityBag.{
                          page:
                            Rect.{
                              top:
                                ghost.currentRect.page.top
                                + (
                                  nextPoint.page.y - ghost.currentPoint.page.y
                                ),
                              bottom:
                                ghost.currentRect.page.bottom
                                + (
                                  nextPoint.page.y - ghost.currentPoint.page.y
                                ),
                              left:
                                ghost.currentRect.page.left
                                + (
                                  nextPoint.page.x - ghost.currentPoint.page.x
                                ),
                              right:
                                ghost.currentRect.page.right
                                + (
                                  nextPoint.page.x - ghost.currentPoint.page.x
                                ),
                            },
                          viewport:
                            Rect.{
                              top:
                                ghost.currentRect.viewport.top
                                + (
                                  nextPoint.viewport.y
                                  - ghost.currentPoint.viewport.y
                                ),
                              bottom:
                                ghost.currentRect.viewport.bottom
                                + (
                                  nextPoint.viewport.y
                                  - ghost.currentPoint.viewport.y
                                ),
                              left:
                                ghost.currentRect.viewport.left
                                + (
                                  nextPoint.viewport.x
                                  - ghost.currentPoint.viewport.x
                                ),
                              right:
                                ghost.currentRect.viewport.right
                                + (
                                  nextPoint.viewport.x
                                  - ghost.currentPoint.viewport.x
                                ),
                            },
                        };

                      let nextDirection =
                        switch (ghost.axis) {
                        | X =>
                          Geometry.getDirection(
                            ~was=ghost.currentPoint.viewport.x,
                            ~is=nextPoint.viewport.x,
                          )
                        | Y =>
                          Geometry.getDirection(
                            ~was=ghost.currentPoint.viewport.y,
                            ~is=nextPoint.viewport.y,
                          )
                        };

                      let nextGhost = {
                        ...ghost,
                        targetDroppable,
                        targetingOriginalDroppable,
                        direction:
                          switch (nextDirection) {
                          | Some(direction) => Some(direction)
                          | None => ghost.direction
                          },
                        currentRect: nextRect,
                        currentPoint: nextPoint,
                        delta: {
                          x: nextPoint.page.x - ghost.departurePoint.page.x,
                          y: nextPoint.page.y - ghost.departurePoint.page.y,
                        },
                      };

                      RecalculateLayout(nextGhost) |> send;
                    }),
                  );
              }
            ),
          )
        | Dropping(_)
        | StandBy => ReasonReact.NoUpdate
        }

      | UpdateScrollPosition =>
        ReasonReact.SideEffects(
          (
            ({state, send}) =>
              switch (state.status) {
              | Dragging(ghost, _) =>
                let scrollable =
                  state.droppables^
                  |. Map.reduce(
                       None,
                       (
                         scrollable: option(ScrollableElement.t),
                         _,
                         droppable,
                       ) =>
                       switch (scrollable, droppable.scrollable) {
                       | (Some(scrollable), Some(scrollable')) =>
                         if (Geometry.contains(
                               ~parent=scrollable'.geometry.rect.page,
                               ~child=scrollable.geometry.rect.page,
                             )) {
                           Some(scrollable);
                         } else {
                           Some(scrollable');
                         }
                       | (None, Some(scrollable')) =>
                         if (ghost.currentPoint.page
                             |. Geometry.isWithin(
                                  scrollable'.geometry.rect.page,
                                )) {
                           Some(scrollable');
                         } else {
                           None;
                         }
                       | (Some(scrollable), None) => Some(scrollable)
                       | (None, None) => None
                       }
                     );

                let scroller =
                  Scroller.getScroller(
                    ~point=ghost.currentPoint,
                    ~viewport=state.viewport^ |. Option.getExn,
                    ~scroll=state.scroll^ |. Option.getExn,
                    ~scrollable,
                  );

                switch (scroller) {
                | Some(Window(requestWindowScroll)) =>
                  switch (state.scheduledWindowScrollFrameId^) {
                  | Some(frameId) =>
                    frameId |> Webapi.cancelAnimationFrame;
                    state.scheduledWindowScrollFrameId := None;
                  | None => ()
                  };

                  state.scheduledWindowScrollFrameId :=
                    requestWindowScroll(() => {
                      let scroll = state.scroll^ |. Option.getExn;

                      let nextScrollPosition =
                        Scrollable.Window.getScrollPosition();

                      let nextScroll =
                        Scroll.{
                          ...scroll,
                          current: nextScrollPosition,
                          delta: {
                            x: nextScrollPosition.x - scroll.initial.x,
                            y: nextScrollPosition.y - scroll.initial.y,
                          },
                        };

                      let nextPoint =
                        RelativityBag.{
                          page:
                            Point.{
                              x:
                                ghost.currentPoint.page.x
                                + nextScroll.current.x
                                - scroll.current.x,
                              y:
                                ghost.currentPoint.page.y
                                + nextScroll.current.y
                                - scroll.current.y,
                            },
                          viewport:
                            Point.{
                              x: ghost.currentPoint.viewport.x,
                              y: ghost.currentPoint.viewport.y,
                            },
                        };

                      let delta =
                        Delta.{
                          x: nextScrollPosition.x - scroll.current.x,
                          y: nextScrollPosition.y - scroll.current.y,
                        };

                      state.droppables :=
                        state.droppables^
                        |. Map.map(droppable =>
                             switch (droppable.scrollable) {
                             | Some(scrollable) => {
                                 ...droppable,
                                 geometry:
                                   droppable.geometry
                                   |. Option.map(geometry =>
                                        {
                                          ...geometry,
                                          rect:
                                            geometry.rect
                                            |> Geometry.shiftViewportRect(
                                                 delta,
                                               ),
                                        }
                                      ),
                                 scrollable:
                                   Some({
                                     ...scrollable,
                                     geometry: {
                                       ...scrollable.geometry,
                                       rect:
                                         scrollable.geometry.rect
                                         |> Geometry.shiftViewportRect(delta),
                                     },
                                   }),
                               }
                             | None => {
                                 ...droppable,
                                 geometry:
                                   droppable.geometry
                                   |. Option.map(geometry =>
                                        {
                                          ...geometry,
                                          rect:
                                            geometry.rect
                                            |> Geometry.shiftViewportRect(
                                                 delta,
                                               ),
                                        }
                                      ),
                               }
                             }
                           );

                      state.scroll := Some(nextScroll);

                      UpdateGhostPosition(nextPoint) |> send;
                    });

                | Some(Element(requestElementScroll)) =>
                  switch (state.scheduledScrollableElementScrollFrameId^) {
                  | Some(frameId) =>
                    frameId |> Webapi.cancelAnimationFrame;
                    state.scheduledScrollableElementScrollFrameId := None;
                  | None => ()
                  };

                  state.scheduledScrollableElementScrollFrameId :=
                    requestElementScroll(scrollable => {
                      let nextScrollPosition =
                        scrollable.element
                        |> Scrollable.Element.getScrollPosition;

                      let nextScroll =
                        Scroll.{
                          ...scrollable.scroll,
                          current: nextScrollPosition,
                          delta:
                            Delta.{
                              x:
                                nextScrollPosition.x
                                - scrollable.scroll.initial.x,
                              y:
                                nextScrollPosition.y
                                - scrollable.scroll.initial.y,
                            },
                        };

                      let delta =
                        Delta.{
                          x:
                            nextScrollPosition.x - scrollable.scroll.current.x,
                          y:
                            nextScrollPosition.y - scrollable.scroll.current.y,
                        };

                      state.droppables :=
                        state.droppables^
                        |. Map.map(droppable =>
                             switch (droppable.scrollable) {
                             | Some(scrollable')
                                 when
                                   scrollable'.element === scrollable.element => {
                                 ...droppable,
                                 geometry:
                                   droppable.geometry
                                   |. Option.map(geometry =>
                                        {
                                          ...geometry,
                                          rect:
                                            geometry.rect
                                            |> Geometry.shiftRects(delta),
                                        }
                                      ),
                                 scrollable:
                                   Some({...scrollable, scroll: nextScroll}),
                               }
                             | Some(scrollable')
                                 when
                                   Geometry.contains(
                                     ~parent=scrollable.geometry.rect.page,
                                     ~child=scrollable'.geometry.rect.page,
                                   ) => {
                                 ...droppable,
                                 geometry:
                                   droppable.geometry
                                   |. Option.map(geometry =>
                                        {
                                          ...geometry,
                                          rect:
                                            geometry.rect
                                            |> Geometry.shiftRects(delta),
                                        }
                                      ),
                               }
                             | Some(_)
                             | None => droppable
                             }
                           );

                      state.scheduledLayoutRecalculationFrameId :=
                        Some(
                          Webapi.requestCancellableAnimationFrame(_ =>
                            RecalculateLayout(ghost) |> send
                          ),
                        );
                    });

                | None => ()
                };

              | Dropping(_)
              | StandBy => ()
              }
          ),
        )

      | RecalculateLayout(ghost) =>
        switch (state.status) {
        | Dragging(_, subscriptions) =>
          let (draggables, animate) =
            switch (ghost.targetDroppable) {
            | None =>
              let draggables =
                state.draggables^
                |. Map.map(draggable => {...draggable, shift: None});
              (draggables, []);
            | Some(targetDroppableId) =>
              let droppable =
                state.droppables^ |. Map.getExn(targetDroppableId);

              state.draggables^
              |. Map.reduce(
                   (state.draggables^, []),
                   ((draggables, animate), id, draggable) =>
                   switch (draggable.droppableId) {
                   | draggableDroppableId
                       when
                         Cfg.Droppable.eq(
                           targetDroppableId,
                           draggableDroppableId,
                         )
                         && ghost.targetingOriginalDroppable =>
                     let geometry = draggable.geometry |. Option.getExn;
                     let scroll = state.scroll^ |. Option.getExn;

                     let shiftedDraggableRect =
                       Geometry.shiftInternalSibling(
                         ghost.axis,
                         ghost.dimensions,
                         geometry,
                         scroll,
                         droppable.scrollable,
                         draggable.shift,
                       );
                     let isAfore =
                       Geometry.isAforeAdjusted(
                         ~subject=ghost.currentRect.page,
                         ~comparand=shiftedDraggableRect.page,
                         ~axis=ghost.axis,
                         ~direction=ghost.direction,
                       );
                     let wasAfore =
                       Geometry.isAfore(
                         ~subject=ghost.departureRect.page,
                         ~comparand=geometry.rect.page,
                         ~axis=ghost.axis,
                       );
                     switch (draggable.shift, isAfore, wasAfore) {
                     /* Dragging this one, no changes here */
                     | (_, _, _)
                         when
                           Cfg.Draggable.eq(draggable.id, ghost.draggableId) => (
                         draggables,
                         animate,
                       )
                     /* This one is in the middle of transition, ignoring */
                     | (_, _, _) when draggable.animating => (
                         draggables,
                         animate,
                       )
                     /* Ghost is above but initially was below: since it's already Omega - ignoring */
                     | (Some(Omega), true, false) => (draggables, animate)
                     /* Ghost is above but initially was below: adding to animating list since it's going to be animated on the next render */
                     | (_, true, false) => (
                         draggables
                         |. Map.set(
                              id,
                              {
                                ...draggable,
                                shift: Some(Omega),
                                animating: true,
                              },
                            ),
                         [id, ...animate],
                       )
                     /* Ghost is below but initially was above: since it's already Alpha - ignoring */
                     | (Some(Alpha), false, true) => (draggables, animate)
                     /* Ghost is below but initially was above: adding to animating list since it's going to be animated on the next render */
                     | (_, false, true) => (
                         draggables
                         |. Map.set(
                              id,
                              {
                                ...draggable,
                                shift: Some(Alpha),
                                animating: true,
                              },
                            ),
                         [id, ...animate],
                       )
                     /* If we got here, draggable should go to original position: if it was shifted — animating it */
                     | (Some(_), _, _) => (
                         draggables
                         |. Map.set(
                              id,
                              {...draggable, shift: None, animating: true},
                            ),
                         [id, ...animate],
                       )
                     /* Draggable is in original position, no updates required */
                     | (None, _, _) => (draggables, animate)
                     };

                   | draggableDroppableId
                       when
                         Cfg.Droppable.eq(
                           targetDroppableId,
                           draggableDroppableId,
                         )
                         && ! ghost.targetingOriginalDroppable =>
                     let geometry = draggable.geometry |. Option.getExn;
                     let scroll = state.scroll^ |. Option.getExn;

                     let shiftedDraggableRect =
                       Geometry.shiftExternalSibling(
                         ghost.axis,
                         ghost.dimensions,
                         geometry,
                         scroll,
                         droppable.scrollable,
                         draggable.shift,
                       );
                     let isAfore =
                       Geometry.isAforeAdjusted(
                         ~subject=ghost.currentRect.page,
                         ~comparand=shiftedDraggableRect.page,
                         ~axis=ghost.axis,
                         ~direction=ghost.direction,
                       );
                     switch (draggable.animating, draggable.shift, isAfore) {
                     | (true, _, _) => (draggables, animate)
                     | (false, Some(Omega), true) => (draggables, animate)
                     | (false, _, true) => (
                         draggables
                         |. Map.set(
                              id,
                              {
                                ...draggable,
                                shift: Some(Omega),
                                animating: true,
                              },
                            ),
                         [id, ...animate],
                       )
                     | (false, Some(Alpha), false) => (draggables, animate)
                     | (false, _, false) => (
                         draggables
                         |. Map.set(
                              id,
                              {
                                ...draggable,
                                shift: Some(Alpha),
                                animating: true,
                              },
                            ),
                         [id, ...animate],
                       )
                     };
                   | _ => (
                       draggables |. Map.set(id, {...draggable, shift: None}),
                       animate,
                     )
                   }
                 );
            };

          ReasonReact.UpdateWithSideEffects(
            {
              ...state,
              status: Dragging(ghost, subscriptions),
              draggables: ref(draggables),
            },
            (
              ({send}) => {
                UpdateScrollPosition |> send;
                Js.Global.setTimeout(
                  () => ResetAnimations(animate) |> send,
                  Style.(animationDuration + resetAnimationsFactor),
                )
                |> ignore;
              }
            ),
          );
        | Dropping(_)
        | StandBy => ReasonReact.NoUpdate
        }

      | ResetAnimations(draggableIds) =>
        switch (draggableIds) {
        | [] => ReasonReact.NoUpdate
        | _ as ids =>
          ReasonReact.SideEffects(
            (
              ({state}) =>
                state.draggables :=
                  ids
                  |. List.reduceU(state.draggables^, (. map, id) =>
                       map
                       |. Map.updateU(id, (. draggable) =>
                            switch (draggable) {
                            | Some(draggable) =>
                              Some(
                                DraggableBag.{...draggable, animating: false},
                              )
                            | None => None
                            }
                          )
                     )
            ),
          )
        }

      | PrepareDrop =>
        ReasonReact.SideEffects(
          (
            ({state, send}) => {
              switch (state.scheduledLayoutRecalculationFrameId^) {
              | Some(frameId) =>
                frameId |> Webapi.cancelAnimationFrame;
                state.scheduledLayoutRecalculationFrameId := None;
              | None => ()
              };
              switch (state.scheduledWindowScrollFrameId^) {
              | Some(frameId) =>
                frameId |> Webapi.cancelAnimationFrame;
                state.scheduledWindowScrollFrameId := None;
              | None => ()
              };
              switch (state.scheduledScrollableElementScrollFrameId^) {
              | Some(frameId) =>
                frameId |> Webapi.cancelAnimationFrame;
                state.scheduledScrollableElementScrollFrameId := None;
              | None => ()
              };

              StartDropping |> send;
            }
          ),
        )

      | StartDropping =>
        switch (state.status) {
        | Dragging(ghost, _) =>
          let (ghost, result) =
            switch (ghost.targetDroppable) {
            | None =>
              let droppable =
                state.droppables^ |. Map.getExn(ghost.originalDroppable);
              let scrollableDelta =
                switch (droppable) {
                | {scrollable: Some(scrollable)} =>
                  Delta.{
                    x: scrollable.scroll.delta.x,
                    y: scrollable.scroll.delta.y,
                  }
                | {scrollable: None} => Delta.{x: 0, y: 0}
                };
              let nextGhost = {
                ...ghost,
                delta:
                  switch (ghost.axis) {
                  | X => {x: 0 - scrollableDelta.x, y: 0}
                  | Y => {x: 0, y: 0 - scrollableDelta.y}
                  },
              };

              (nextGhost, DropResult.NoChanges);

            | Some(targetDroppableId) when ghost.targetingOriginalDroppable =>
              let droppable =
                state.droppables^ |. Map.getExn(targetDroppableId);
              let scroll = state.scroll^ |. Option.getExn;
              let scrollableDelta =
                switch (droppable) {
                | {scrollable: Some(scrollable)} =>
                  Delta.{
                    x: scrollable.scroll.delta.x,
                    y: scrollable.scroll.delta.y,
                  }
                | {scrollable: None} => Delta.{x: 0, y: 0}
                };

              let sortedDraggables =
                state.draggables^
                |. Map.keep((_, draggable) =>
                     Cfg.Droppable.eq(
                       draggable.droppableId,
                       targetDroppableId,
                     )
                   )
                |. Map.reduce(
                     [||],
                     (acc, id, draggable) => {
                       let geometry = draggable.geometry |. Option.getExn;

                       if (Cfg.Draggable.eq(id, ghost.draggableId)) {
                         acc
                         |. Array.concat([|
                              DropResult.{
                                id,
                                trait: Ghost,
                                rect: ghost.currentRect,
                                margins: geometry.margins,
                              },
                            |]);
                       } else {
                         acc
                         |. Array.concat([|
                              {
                                id,
                                trait: Item(draggable.shift),
                                rect:
                                  Geometry.shiftInternalSibling(
                                    ghost.axis,
                                    ghost.dimensions,
                                    geometry,
                                    scroll,
                                    droppable.scrollable,
                                    draggable.shift,
                                  ),
                                margins: geometry.margins,
                              },
                            |]);
                       };
                     },
                   )
                |. SortArray.stableSortBy((d1, d2) =>
                     switch (d1.trait, d2.trait, ghost.axis) {
                     | (Ghost, Item(Some(Alpha)), _) => 1
                     | (Ghost, Item(Some(Omega)), _) => (-1)
                     | (Ghost, Item(None), X) =>
                       ghost.departureRect.page.left
                       - (d2.rect.page.left + scrollableDelta.x)
                     | (Ghost, Item(None), Y) =>
                       ghost.departureRect.page.top
                       - (d2.rect.page.top + scrollableDelta.y)
                     | (Item(Some(Alpha)), Ghost, _) => (-1)
                     | (Item(Some(Omega)), Ghost, _) => 1
                     | (Item(None), Ghost, X) =>
                       d1.rect.page.left
                       + scrollableDelta.x
                       - ghost.departureRect.page.left
                     | (Item(None), Ghost, Y) =>
                       d1.rect.page.top
                       + scrollableDelta.y
                       - ghost.departureRect.page.top
                     | (Item(_), Item(_), X) =>
                       d1.rect.page.left - d2.rect.page.left
                     | (Item(_), Item(_), Y) =>
                       d1.rect.page.top - d2.rect.page.top
                     | (Ghost, Ghost, _) => 0 /* impossible */
                     }
                   );

              let ghostIndex =
                sortedDraggables
                |> Js.Array.findIndex(item =>
                     DropResult.(
                       switch (item.trait) {
                       | Ghost => true
                       | Item(_) => false
                       }
                     )
                   );

              switch (sortedDraggables |. Array.get(ghostIndex - 1)) {
              | Some(draggable) => (
                  {
                    ...ghost,
                    delta:
                      switch (ghost.axis) {
                      | X => {
                          x:
                            /* To the right bound of the neighbour on the left */
                            draggable.rect.page.right
                            /* add its right and ghost's left margins */
                            + (draggable.margins.right + ghost.margins.left)
                            /* add distance between the pointer and ghost's left bound */
                            + (
                              ghost.departurePoint.page.x
                              - ghost.departureRect.page.left
                            )
                            /* finally, substract distance from the left side of the page */
                            - ghost.departurePoint.page.x,
                          y: 0,
                        }
                      | Y => {
                          x: 0,
                          y:
                            /* To the bottom bound of the neighbour above */
                            draggable.rect.page.bottom
                            /* add its bottom and ghost's top margins */
                            + (draggable.margins.bottom + ghost.margins.top)
                            /* add distance between the pointer and ghost's top bound */
                            + (
                              ghost.departurePoint.page.y
                              - ghost.departureRect.page.top
                            )
                            /* finally, substract distance from the top of the page */
                            - ghost.departurePoint.page.y,
                        }
                      },
                  },
                  SameTarget(
                    ghost.draggableId,
                    targetDroppableId,
                    sortedDraggables |. Array.map(item => item.id),
                  ),
                )
              | None =>
                switch (sortedDraggables |. Array.get(ghostIndex + 1)) {
                | Some(draggable) => (
                    {
                      ...ghost,
                      delta:
                        switch (ghost.axis) {
                        | X => {
                            x:
                              /* From the left bound of the neighbour on the right */
                              draggable.rect.page.left
                              /* substract its left and ghost's right margins */
                              - (draggable.margins.left + ghost.margins.right)
                              /* substract distance between ghost's right bound and the pointer */
                              - (
                                ghost.departureRect.page.right
                                - ghost.departurePoint.page.x
                              )
                              /* finally, substract distance from the left side of the page */
                              - ghost.departurePoint.page.x,
                            y: 0,
                          }
                        | Y => {
                            x: 0,
                            y:
                              /* From the top bound of the neighbour below */
                              draggable.rect.page.top
                              /* substract its top and ghost's bottom margins */
                              - (draggable.margins.top + ghost.margins.bottom)
                              /* substract distance between ghost's bottom bound and the pointer */
                              - (
                                ghost.departureRect.page.bottom
                                - ghost.departurePoint.page.y
                              )
                              /* finally, substract distance from the top of the page */
                              - ghost.departurePoint.page.y,
                          }
                        },
                    },
                    SameTarget(
                      ghost.draggableId,
                      targetDroppableId,
                      sortedDraggables |. Array.map(item => item.id),
                    ),
                  )
                | None => ({
                             ...ghost,
                             delta: {
                               x: 0,
                               y: 0,
                             },
                           }, NoChanges)
                }
              };

            | Some(targetDroppableId) =>
              let droppable =
                state.droppables^ |. Map.getExn(targetDroppableId);
              let scroll = state.scroll^ |. Option.getExn;
              let scrollableDelta =
                switch (droppable) {
                | {scrollable: Some(scrollable)} =>
                  Delta.{
                    x: scrollable.scroll.delta.x,
                    y: scrollable.scroll.delta.y,
                  }
                | {scrollable: None} => Delta.{x: 0, y: 0}
                };

              let sortedDraggables =
                state.draggables^
                |. Map.keep((_, draggable) =>
                     Cfg.Droppable.eq(
                       draggable.droppableId,
                       targetDroppableId,
                     )
                     || Cfg.Draggable.eq(draggable.id, ghost.draggableId)
                   )
                |. Map.reduce(
                     [||],
                     (acc, id, draggable) => {
                       let geometry = draggable.geometry |. Option.getExn;

                       if (Cfg.Draggable.eq(id, ghost.draggableId)) {
                         acc
                         |. Array.concat([|
                              DropResult.{
                                id,
                                trait: Ghost,
                                rect: ghost.currentRect,
                                margins: geometry.margins,
                              },
                            |]);
                       } else {
                         acc
                         |. Array.concat([|
                              {
                                id,
                                trait: Item(draggable.shift),
                                rect:
                                  Geometry.shiftExternalSibling(
                                    ghost.axis,
                                    ghost.dimensions,
                                    geometry,
                                    scroll,
                                    droppable.scrollable,
                                    draggable.shift,
                                  ),
                                margins: geometry.margins,
                              },
                            |]);
                       };
                     },
                   )
                |. SortArray.stableSortBy((d1, d2) =>
                     switch (d1.trait, d2.trait, ghost.axis) {
                     | (Ghost, Item(Some(Alpha)), _) => 1
                     | (Ghost, Item(Some(Omega)), _) => (-1)
                     | (Ghost, Item(None), _) => 1
                     | (Item(Some(Alpha)), Ghost, _) => (-1)
                     | (Item(Some(Omega)), Ghost, _) => 1
                     | (Item(None), Ghost, _) => (-1)
                     | (Item(_), Item(_), X) =>
                       d1.rect.page.left - d2.rect.page.left
                     | (Item(_), Item(_), Y) =>
                       d1.rect.page.top - d2.rect.page.top
                     | (Ghost, Ghost, _) => 0 /* impossible */
                     }
                   );

              let ghostIndex =
                sortedDraggables
                |> Js.Array.findIndex(item =>
                     DropResult.(
                       switch (item.trait) {
                       | Ghost => true
                       | Item(_) => false
                       }
                     )
                   );

              switch (sortedDraggables |. Array.get(ghostIndex - 1)) {
              | Some(draggable) => (
                  {
                    ...ghost,
                    delta:
                      switch (ghost.axis) {
                      | X => {
                          x:
                            /* To the right bound of the neighbour on the left */
                            draggable.rect.page.right
                            /* add its right and ghost's left margins */
                            + (draggable.margins.right + ghost.margins.left)
                            /* add distance between the pointer and ghost's left bound */
                            + (
                              ghost.departurePoint.page.x
                              - ghost.departureRect.page.left
                            )
                            /* finally, substract distance from the left side of the page */
                            - ghost.departurePoint.page.x,
                          y:
                            draggable.rect.page.top
                            - ghost.departureRect.page.top
                            - (scroll.delta.y + scrollableDelta.y),
                        }
                      | Y => {
                          x:
                            draggable.rect.page.left
                            - ghost.departureRect.page.left
                            - (scroll.delta.x + scrollableDelta.x),
                          y:
                            /* To the bottom bound of the neighbour above */
                            draggable.rect.page.bottom
                            /* add its bottom and ghost's top margins */
                            + (draggable.margins.bottom + ghost.margins.top)
                            /* add distance between the pointer and ghost's top bound */
                            + (
                              ghost.departurePoint.page.y
                              - ghost.departureRect.page.top
                            )
                            /* finally, substract distance from the top of the page */
                            - ghost.departurePoint.page.y,
                        }
                      },
                  },
                  NewTarget(
                    ghost.draggableId,
                    {prev: ghost.originalDroppable, next: targetDroppableId},
                    sortedDraggables |. Array.map(item => item.id),
                  ),
                )
              | None =>
                switch (sortedDraggables |. Array.get(ghostIndex + 1)) {
                | Some(draggable) => (
                    {
                      ...ghost,
                      delta:
                        switch (ghost.axis) {
                        | X => {
                            x:
                              /* From the left bound of the neighbour on the right */
                              draggable.rect.page.left
                              /* substract its left and ghost's right margins */
                              - (draggable.margins.left + ghost.margins.right)
                              /* substract distance between ghost's right bound and the pointer */
                              - (
                                ghost.departureRect.page.right
                                - ghost.departurePoint.page.x
                              )
                              /* finally, substract distance from the left side of the page */
                              - ghost.departurePoint.page.x,
                            y:
                              draggable.rect.page.top
                              - ghost.departureRect.page.top
                              - (scroll.delta.y + scrollableDelta.y),
                          }
                        | Y => {
                            x:
                              draggable.rect.page.left
                              - ghost.departureRect.page.left
                              - (scroll.delta.x + scrollableDelta.x),
                            y:
                              /* From the top bound of the neighbour below */
                              draggable.rect.page.top
                              /* substract its top and ghost's bottom margins */
                              - (draggable.margins.top + ghost.margins.bottom)
                              /* substract distance between ghost's bottom bound and the pointer */
                              - (
                                ghost.departureRect.page.bottom
                                - ghost.departurePoint.page.y
                              )
                              /* finally, substract distance from the top of the page */
                              - ghost.departurePoint.page.y,
                          }
                        },
                    },
                    NewTarget(
                      ghost.draggableId,
                      {
                        prev: ghost.originalDroppable,
                        next: targetDroppableId,
                      },
                      sortedDraggables |. Array.map(item => item.id),
                    ),
                  )

                /* Dropping onto empty droppable */
                | None =>
                  let droppable =
                    Map.getExn(state.droppables^, targetDroppableId).geometry
                    |. Option.getExn;
                  (
                    {
                      ...ghost,
                      delta:
                        switch (ghost.axis) {
                        | X => {
                            x:
                              /* From the right bound of the droppable */
                              droppable.rect.page.right
                              /* substract the right border and padding of droppable */
                              - (
                                droppable.borders.right
                                + droppable.paddings.right
                              )
                              /* substract distance between ghost's right bound and the pointer */
                              - (
                                ghost.departureRect.page.right
                                - ghost.departurePoint.page.x
                              )
                              /* add ghost's width since ghost pushed the right side of the droppable to the left */
                              + ghost.dimensions.width
                              /* finally, substract distance from the left side of the page */
                              - ghost.departurePoint.page.x,
                            y:
                              /* To the top bound of the droppable */
                              droppable.rect.page.top
                              /* add the top border and padding of the droppable */
                              + (
                                droppable.borders.top + droppable.paddings.top
                              )
                              /* add distance between ghost's top bound and the pointer */
                              + (
                                ghost.departurePoint.page.y
                                - ghost.departureRect.page.top
                              )
                              /* finally, substract distance from the top of the page */
                              - ghost.departurePoint.page.y,
                          }
                        | Y => {
                            x:
                              /* To the left bound of the droppable */
                              droppable.rect.page.left
                              /* add the left border and padding of the droppable */
                              + (
                                droppable.borders.left
                                + droppable.paddings.left
                              )
                              /* add distance between ghost's left bound and the pointer */
                              + (
                                ghost.departurePoint.page.x
                                - ghost.departureRect.page.left
                              )
                              /* finally, substract distance from the left side of the page */
                              - ghost.departurePoint.page.x,
                            y:
                              /* From the bottom bound of the droppable */
                              droppable.rect.page.bottom
                              /* substract the bottom border and padding of the droppable */
                              - (
                                droppable.borders.bottom
                                + droppable.paddings.bottom
                              )
                              /* substract distance between ghost's bottom bound and the pointer */
                              - (
                                ghost.departureRect.page.bottom
                                - ghost.departurePoint.page.y
                              )
                              /* add ghost's height since ghost pushed the bottom side down */
                              + ghost.dimensions.height
                              /* finally, substract distance from the top of the page */
                              - ghost.departurePoint.page.y,
                          }
                        },
                    },
                    NewTarget(
                      ghost.draggableId,
                      {
                        prev: ghost.originalDroppable,
                        next: targetDroppableId,
                      },
                      sortedDraggables |. Array.map(item => item.id),
                    ),
                  );
                }
              };
            };

          ReasonReact.UpdateWithSideEffects(
            {...state, status: Dropping(ghost)},
            (
              ({send}) =>
                Js.Global.setTimeout(
                  () => FinishDropping(result) |> send,
                  Style.(animationDuration + finishDropFactor),
                )
                |> ignore
            ),
          );

        | Dropping(_)
        | StandBy => ReasonReact.NoUpdate
        }

      | CancelDrag =>
        switch (state.status) {
        | Dragging(ghost, _) =>
          let droppable =
            state.droppables^ |. Map.getExn(ghost.originalDroppable);

          let ghost =
            switch (droppable) {
            | {scrollable: Some(scrollable)} => {
                ...ghost,
                delta: {
                  x: 0 - scrollable.scroll.delta.x,
                  y: 0 - scrollable.scroll.delta.y,
                },
              }
            | {scrollable: None} => {
                ...ghost,
                delta: {
                  x: 0,
                  y: 0,
                },
              }
            };

          let draggables =
            state.draggables^
            |. Map.map(draggable => {...draggable, shift: None});

          ReasonReact.UpdateWithSideEffects(
            {
              ...state,
              status: Dropping(ghost),
              draggables: ref(draggables),
            },
            (
              ({send}) =>
                Js.Global.setTimeout(
                  () => FinishDropping(NoChanges) |> send,
                  Style.(animationDuration + finishDropFactor),
                )
                |> ignore
            ),
          );

        | Dropping(_)
        | StandBy => ReasonReact.NoUpdate
        }

      | FinishDropping(result) =>
        ReasonReact.SideEffects(
          (
            ({send}) => {
              result |> onDrop;
              Reset |> send;
            }
          ),
        )

      | Reset =>
        ReasonReact.Update({...state, status: StandBy, scroll: ref(None)})
      },
    render: ({state, send, handle}) =>
      children(
        Payload.{
          context: {
            status: state.status,
            scroll: state.scroll^,
            target:
              switch (state.status) {
              | Dragging(ghost, _)
              | Dropping(ghost) => ghost.targetDroppable
              | _ => None
              },
            registerDraggable: Handlers.registerDraggable |> handle,
            registerDroppable: Handlers.registerDroppable |> handle,
            disposeDraggable: Handlers.disposeDraggable |> handle,
            disposeDroppable: Handlers.disposeDroppable |> handle,
            getDraggableShift: draggableId =>
              Map.getExn(state.draggables^, draggableId).shift,
            initDrag:
              (
                ~draggableId,
                ~droppableId,
                ~start,
                ~current,
                ~element,
                ~subscriptions,
              ) =>
              PrepareDrag(
                draggableId,
                droppableId,
                start,
                current,
                element,
                subscriptions,
              )
              |> send,
            updateGhostPosition: point => UpdateGhostPosition(point) |> send,
            drop: () => PrepareDrop |> send,
            cancelDrag: () => CancelDrag |> send,
          },
        },
      ),
  };
};
