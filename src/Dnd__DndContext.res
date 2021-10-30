open Dnd__Config
open Dnd__Types

module ReactContext = Dnd__ReactContext

exception MissingContext

let fail = _ => MissingContext->raise

module T = {
  type t<'itemId, 'containerId> = {
    status: Status.t<'itemId, 'containerId>,
    target: option<'containerId>,
    scroll: option<Scroll.t>,
    registerItem: ItemBag.registrationPayload<'itemId, 'containerId> => unit,
    registerContainer: ContainerBag.registrationPayload<'itemId, 'containerId> => unit,
    disposeItem: 'itemId => unit,
    disposeContainer: 'containerId => unit,
    getItemShift: 'itemId => Shift.t,
    startDragging: (
      'itemId,
      'containerId,
      RelativityBag.t<Point.t>,
      RelativityBag.t<Point.t>,
      [#Mouse | #Touch],
    ) => unit,
  }
}

module type T = {
  module Item: DndEntry
  module Container: DndEntry

  type t = T.t<Item.t, Container.t>

  let x: React.Context.t<t>
  let useDnd: unit => t

  module Provider: {
    let make: React.component<{"value": t, "children": React.element}>
    let makeProps: (
      ~value: t,
      ~children: React.element,
      ~key: string=?,
      unit,
    ) => {"value": t, "children": React.element}
  }
}

module Make = (Item: DndEntry, Container: DndEntry) => {
  module Item = Item
  module Container = Container

  type t = T.t<Item.t, Container.t>

  let x: React.Context.t<t> = React.createContext({
    open T
    {
      status: StandBy,
      target: None,
      scroll: None,
      registerItem: fail,
      registerContainer: fail,
      disposeItem: fail,
      disposeContainer: fail,
      getItemShift: fail,
      startDragging: fail,
    }
  })

  module Provider = {
    let make = x->React.Context.provider
    let makeProps = ReactContext.makeProps
  }

  let useDnd = () => React.useContext(x)
}
