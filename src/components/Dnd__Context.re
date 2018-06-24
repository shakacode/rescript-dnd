open Dnd__Config;
open Dnd__Types;

module Html = Dnd__Html;
module Style = Dnd__Style;
module Geometry = Dnd__Geometry;

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
  };

  type action =
    | StartDragging(
        Cfg.Draggable.t,
        Cfg.Droppable.t,
        Point.t,
        Point.t,
        Dom.htmlElement,
        Subscriptions.t,
      )
    | UpdateGhostPosition(
        Ghost.t(Cfg.Draggable.t, Cfg.Droppable.t),
        Point.t,
        Dom.htmlElement,
        Subscriptions.t,
      )
    | ResetAnimations(list(Cfg.Draggable.t))
    | StartDropping(Ghost.t(Cfg.Draggable.t, Cfg.Droppable.t))
    | CancelDropping(Ghost.t(Cfg.Draggable.t, Cfg.Droppable.t))
    | FinishDropping(DropResult.t(Cfg.Draggable.t, Cfg.Droppable.t))
    | Reset;

  module Handlers = {
    let registerDraggable =
        ((draggableId, droppableId, element), {ReasonReact.state}) =>
      switch (state.status) {
      | StandBy =>
        state.draggables :=
          state.draggables^
          |. Map.set(
               draggableId,
               {
                 id: draggableId,
                 droppableId,
                 element,
                 geometry: None,
                 shift: None,
                 animating: false,
               },
             )
      | Dragging(_, _)
      | Dropping(_) => ()
      };

    let registerDroppable =
        ((droppableId, accept, element), {ReasonReact.state}) =>
      switch (state.status) {
      | StandBy =>
        state.droppables :=
          state.droppables^
          |. Map.set(
               droppableId,
               {id: droppableId, accept, element, geometry: None},
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
    },
    reducer: (action, state) =>
      switch (action) {
      | StartDragging(
          draggableId,
          droppableId,
          start,
          current,
          element,
          subscriptions,
        ) =>
        open Webapi.Dom;

        let rect = element |. HtmlElement.getBoundingClientRect;
        let style =
          element
          |. Html.castHtmlElementToElement
          |. Window.getComputedStyle(window);

        let ghost =
          Ghost.{
            draggableId,
            originalDroppable: droppableId,
            targetDroppable: Some(droppableId),
            targetingOriginalDroppable: true,
            direction: Geometry.getDirection(~was=start.y, ~is=current.y),
            dimensions: rect |. Geometry.getDimensions,
            margins: style |. Geometry.getMargins,
            borders: style |. Geometry.getBorders,
            center: rect |. Geometry.getAbsCenter,
            departureRect: rect |. Geometry.getAbsRect,
            currentRect: rect |. Geometry.getAbsRect,
            departurePoint: {
              x: current.x,
              y: current.y,
            },
            currentPoint: {
              x: current.x,
              y: current.y,
            },
            delta: {
              x: 0,
              y: 0,
            },
          };

        state.draggables :=
          state.draggables^
          |. Map.map(draggable =>
               {
                 ...draggable,
                 geometry: Some(draggable.element |> Geometry.getGeometry),
               }
             );

        state.droppables :=
          state.droppables^
          |. Map.map(droppable =>
               {
                 ...droppable,
                 geometry: Some(droppable.element |> Geometry.getGeometry),
               }
             );

        ReasonReact.UpdateWithSideEffects(
          {...state, status: Dragging(ghost, subscriptions)},
          (_ => subscriptions.install()),
        );

      | UpdateGhostPosition(ghost, point, element, subscriptions) =>
        open Webapi.Dom;

        let targetDroppable =
          state.droppables^
          |> Map.valuesToArray
          |> Js.Array.find(droppable =>
               DroppableBag.(
                 droppable.accept
                 |. Option.map(fn => fn(ghost.draggableId))
                 |. Option.getWithDefault(true)
               )
               && Geometry.(
                    point |. isWithin(Option.getExn(droppable.geometry).rect)
                  )
             )
          |. Option.map(droppable => droppable.id);

        let targetingOriginalDroppable =
          switch (targetDroppable) {
          | None => true
          | Some(targetDroppable)
              when Cfg.Droppable.eq(targetDroppable, ghost.originalDroppable) =>
            true
          | Some(_) => false
          };

        let rect = element |. HtmlElement.getBoundingClientRect;

        let ghost = {
          ...ghost,
          targetDroppable,
          targetingOriginalDroppable,
          direction:
            Geometry.getDirection(~was=ghost.currentPoint.y, ~is=point.y),
          center: rect |. Geometry.getAbsCenter,
          currentRect: rect |. Geometry.getAbsRect,
          currentPoint: {
            x: point.x,
            y: point.y,
          },
          delta: {
            x: point.x - ghost.departurePoint.x,
            y: point.y - ghost.departurePoint.y,
          },
        };

        let (draggables, animate) =
          state.draggables^
          |. Map.reduce(
               (state.draggables^, []),
               ((draggables, animate), id, draggable) =>
               switch (ghost.targetDroppable, draggable.droppableId) {
               | (Some(targetDroppable), draggableDroppable)
                   when
                     Cfg.Droppable.eq(targetDroppable, draggableDroppable)
                     && ghost.targetingOriginalDroppable =>
                 let shiftedDraggableRect =
                   ghost.dimensions
                   |. Geometry.shiftInternalSibling(
                        draggable.geometry |. Option.getExn,
                        draggable.shift,
                      );
                 let isAbove =
                   ghost.currentRect
                   |. Geometry.isAboveAdjusted(
                        shiftedDraggableRect,
                        ghost.direction,
                      );
                 let wasAbove =
                   ghost.departureRect
                   |. Geometry.isAbove(
                        Option.getExn(draggable.geometry).rect,
                      );
                 switch (draggable.shift, isAbove, wasAbove) {
                 /* Dragging this one, no changes here */
                 | (_, _, _)
                     when Cfg.Draggable.eq(draggable.id, ghost.draggableId) => (
                     draggables,
                     animate,
                   )
                 /* This one is in the middle of transition, ignoring */
                 | (_, _, _) when draggable.animating => (
                     draggables,
                     animate,
                   )
                 /* Ghost is above but was below initially: since it's already Omega - ignoring */
                 | (Some(Omega), true, false) => (draggables, animate)
                 /* Ghost is above but was below initially: adding to animating array since it's going to be animated on the next render */
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
                 /* Ghost is below but was above initially: since it's already Alpha - ignoring */
                 | (Some(Alpha), false, true) => (draggables, animate)
                 /* Ghost is below but was above initially: adding to animating array since it's going to be animated on the next render */
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
                 /* If it gets here, draggable should go to original position: if it was shifted — animating it */
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

               | (Some(ghostTargetDroppable), draggableDroppable)
                   when
                     Cfg.Droppable.eq(
                       ghostTargetDroppable,
                       draggableDroppable,
                     )
                     && ! ghost.targetingOriginalDroppable =>
                 let shiftedDraggableRect =
                   ghost.dimensions
                   |. Geometry.shiftExternalSibling(
                        draggable.geometry |. Option.getExn,
                        draggable.shift,
                      );
                 let isAbove =
                   ghost.currentRect
                   |. Geometry.isAboveAdjusted(
                        shiftedDraggableRect,
                        ghost.direction,
                      );
                 switch (draggable.animating, draggable.shift, isAbove) {
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

        state.draggables := draggables;

        switch (animate) {
        | [] =>
          ReasonReact.Update({
            ...state,
            status: Dragging(ghost, subscriptions),
          })
        | _ as ids =>
          ReasonReact.UpdateWithSideEffects(
            {...state, status: Dragging(ghost, subscriptions)},
            (
              ({send}) =>
                Js.Global.setTimeout(
                  () => ResetAnimations(ids) |> send,
                  Style.(animationDuration + resetAnimationsFactor),
                )
                |> ignore
            ),
          )
        };

      | ResetAnimations(draggableIds) =>
        ReasonReact.SideEffects(
          (
            ({state}) =>
              state.draggables :=
                draggableIds
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

      | StartDropping(ghost) =>
        let (ghost, result) =
          switch (ghost.targetDroppable) {
          | None => ({
                       ...ghost,
                       delta: {
                         x: 0,
                         y: 0,
                       },
                     }, DropResult.NoChanges)

          | Some(targetDroppableId) when ghost.targetingOriginalDroppable =>
            let sortedDraggables =
              state.draggables^
              |. Map.keep((_, draggable) =>
                   Cfg.Droppable.eq(draggable.droppableId, targetDroppableId)
                 )
              |. Map.reduce(
                   [||],
                   (acc, id, draggable) => {
                     let geometry = draggable.geometry |. Option.getExn;
                     switch (draggable.shift) {
                     | _ when Cfg.Draggable.eq(id, ghost.draggableId) =>
                       acc
                       |. Array.concat([|
                            DropResult.{
                              id,
                              trait: Ghost,
                              rect: ghost.currentRect,
                              margins: geometry.margins,
                            },
                          |])
                     | _ as shift =>
                       acc
                       |. Array.concat([|
                            {
                              id,
                              trait: Item(shift),
                              rect:
                                ghost.dimensions
                                |. Geometry.shiftInternalSibling(
                                     geometry,
                                     shift,
                                   ),
                              margins: geometry.margins,
                            },
                          |])
                     };
                   },
                 )
              |. SortArray.stableSortBy((d1, d2) =>
                   switch (d1.trait, d2.trait) {
                   | (Ghost, Item(Some(Alpha))) => 1
                   | (Ghost, Item(Some(Omega))) => (-1)
                   | (Ghost, Item(None)) =>
                     ghost.departureRect.top - d2.rect.top
                   | (Item(Some(Alpha)), Ghost) => (-1)
                   | (Item(Some(Omega)), Ghost) => 1
                   | (Item(None), Ghost) =>
                     d1.rect.top - ghost.departureRect.top
                   | (Item(_), Item(_)) => d1.rect.top - d2.rect.top
                   | (Ghost, Ghost) => 0 /* impossible */
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
                  delta: {
                    x: 0,
                    y:
                      /* To bottom bound of neighbour above */
                      draggable.rect.bottom
                      /* add its bottom and ghost's top margins */
                      + (draggable.margins.bottom + ghost.margins.top)
                      /* add distance between pointer and ghost's top bound */
                      + (ghost.departurePoint.y - ghost.departureRect.top)
                      /* finally, substract distance from page top */
                      - ghost.departurePoint.y,
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
                    delta: {
                      x: 0,
                      y:
                        /* From top bound of neighbour below */
                        draggable.rect.top
                        /* substract its top and ghost's bottom margins */
                        - (draggable.margins.top + ghost.margins.bottom)
                        /* substract distance between ghost's bottom bound and pointer */
                        - (
                          ghost.departureRect.bottom - ghost.departurePoint.y
                        )
                        /* finally, substract distance from page top */
                        - ghost.departurePoint.y,
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
            let sortedDraggables =
              state.draggables^
              |. Map.keep((_, draggable) =>
                   Cfg.Droppable.eq(draggable.droppableId, targetDroppableId)
                   || Cfg.Draggable.eq(draggable.id, ghost.draggableId)
                 )
              |. Map.reduce(
                   [||],
                   (acc, id, draggable) => {
                     let geometry = draggable.geometry |. Option.getExn;
                     switch (draggable.shift) {
                     | _ when Cfg.Draggable.eq(id, ghost.draggableId) =>
                       acc
                       |. Array.concat([|
                            DropResult.{
                              id,
                              trait: Ghost,
                              rect: ghost.currentRect,
                              margins: geometry.margins,
                            },
                          |])
                     | _ as shift =>
                       acc
                       |. Array.concat([|
                            {
                              id,
                              trait: Item(shift),
                              rect:
                                ghost.dimensions
                                |. Geometry.shiftExternalSibling(
                                     geometry,
                                     shift,
                                   ),
                              margins: geometry.margins,
                            },
                          |])
                     };
                   },
                 )
              |. SortArray.stableSortBy((d1, d2) =>
                   switch (d1.trait, d2.trait) {
                   | (Ghost, Item(Some(Alpha))) => 1
                   | (Ghost, Item(Some(Omega))) => (-1)
                   | (Ghost, Item(None)) => 1
                   | (Item(Some(Alpha)), Ghost) => (-1)
                   | (Item(Some(Omega)), Ghost) => 1
                   | (Item(None), Ghost) => (-1)
                   | (Item(_), Item(_)) => d1.rect.top - d2.rect.top
                   | (Ghost, Ghost) => 0 /* impossible */
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
                  delta: {
                    x: 0,
                    y:
                      /* To bottom bound of neighbour above */
                      draggable.rect.bottom
                      /* add its bottom and ghost's top margins */
                      + (draggable.margins.bottom + ghost.margins.top)
                      /* add distance between pointer and ghost's top bound */
                      + (ghost.departurePoint.y - ghost.departureRect.top)
                      /* finally, substract distance from page top */
                      - ghost.departurePoint.y,
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
                    delta: {
                      x: 0,
                      y:
                        /* From top bound of neighbour below */
                        draggable.rect.top
                        /* substract its top and ghost's bottom margins */
                        - (draggable.margins.top + ghost.margins.bottom)
                        /* substract distance between ghost's bottom bound and pointer */
                        - (
                          ghost.departureRect.bottom - ghost.departurePoint.y
                        )
                        /* finally, substract distance from page top */
                        - ghost.departurePoint.y,
                    },
                  },
                  NewTarget(
                    ghost.draggableId,
                    {prev: ghost.originalDroppable, next: targetDroppableId},
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
                    delta: {
                      x: 0,
                      y:
                        /* From bottom bound of droppable */
                        droppable.rect.bottom
                        /* substract bottom border and padding of droppable */
                        - (
                          droppable.borders.bottom + droppable.paddings.bottom
                        )
                        /* substract distance between ghost's bottom bound and pointer */
                        - (
                          ghost.departureRect.bottom - ghost.departurePoint.y
                        )
                        /* add ghost's height since ghost pushed bottom down */
                        + ghost.dimensions.height
                        /* finally, substract distance from page top */
                        - ghost.departurePoint.y,
                    },
                  },
                  NewTarget(
                    ghost.draggableId,
                    {prev: ghost.originalDroppable, next: targetDroppableId},
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

      | CancelDropping(ghost) =>
        state.draggables :=
          state.draggables^
          |. Map.map(draggable => {...draggable, shift: None});

        let ghost = {
          ...ghost,
          delta: {
            x: 0,
            y: 0,
          },
        };

        ReasonReact.UpdateWithSideEffects(
          {...state, status: Dropping(ghost)},
          (
            ({send}) =>
              Js.Global.setTimeout(
                () => FinishDropping(NoChanges) |> send,
                Style.(animationDuration + finishDropFactor),
              )
              |> ignore
          ),
        );

      | FinishDropping(result) =>
        ReasonReact.SideEffects(
          (
            ({send}) => {
              result |> onDrop;
              Reset |> send;
            }
          ),
        )

      | Reset => ReasonReact.Update({...state, status: StandBy})
      },
    render: ({state, send, handle}) =>
      children(
        Payload.{
          context: {
            status: state.status,
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
            startDragging:
              (
                ~draggableId,
                ~droppableId,
                ~start,
                ~current,
                ~element,
                ~subscriptions,
              ) =>
              StartDragging(
                draggableId,
                droppableId,
                start,
                current,
                element,
                subscriptions,
              )
              |> send,
            updateGhostPosition: (~ghost, ~point, ~element, ~subscriptions) =>
              UpdateGhostPosition(ghost, point, element, subscriptions)
              |> send,
            startDropping: ghost => StartDropping(ghost) |> send,
            cancelDropping: ghost => CancelDropping(ghost) |> send,
          },
        },
      ),
  };
};
