open Dnd__React;
open Dnd__Config;
open Dnd__Types;

module Style = Dnd__Style;

module Make = (Cfg: Config) => {
  let component = ReasonReact.statelessComponent("DndDroppable");

  let make =
      (
        ~id as droppableId: Cfg.Droppable.t,
        ~accept: option(Cfg.Draggable.t => bool)=?,
        ~context: Context.t(Cfg.Draggable.t, Cfg.Droppable.t),
        ~className: option(DroppableBag.className)=?,
        children,
      ) => {
    ...component,
    willUnmount: _ => context.disposeDroppable(droppableId),
    render: _ =>
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

                switch (element) {
                | Some(element) =>
                  context.registerDroppable((droppableId, accept, element))
                | None => ()
                };
              },
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
