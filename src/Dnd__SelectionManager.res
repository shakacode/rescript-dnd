open Dnd__Config

module ReactHooks = Dnd__ReactHooks

module Scroll = {
  open Webapi.Dom

  type opts = {"top": float, "left": float, "behavior": string}

  @send external scrollBy: (Dom.window, opts) => unit = "scrollBy"

  let smoothScrollBy = (x: float, y: float) =>
    window->scrollBy({"left": x, "top": y, "behavior": "smooth"})

  // TODO: X scrolling
  let adjust = (el: Dom.element, ~topMarginFactor, ~bottomMarginFactor) => {
    let rect = el->Element.getBoundingClientRect
    let clientHeight = document->Document.documentElement->Element.clientHeight->Float.fromInt

    let shouldScrollUp = rect->DomRect.top < rect->DomRect.height *. topMarginFactor

    let shouldScrollDown =
      clientHeight -. rect->DomRect.bottom < rect->DomRect.height *. bottomMarginFactor

    if shouldScrollUp {
      smoothScrollBy(0., rect->DomRect.top -. rect->DomRect.height *. topMarginFactor)
    } else if shouldScrollDown {
      smoothScrollBy(
        0.,
        rect->DomRect.bottom -. clientHeight +. rect->DomRect.height *. bottomMarginFactor,
      )
    }
  }
}

module Make = (Item: SelectableItem) => {
  module ComparableItem = Belt.Id.MakeComparable(Item)

  type t = {
    register: (Item.t, Nullable.t<Nullable.t<Dom.element>>) => unit,
    dispose: Item.t => unit,
    size: unit => int,
    array: unit => array<Item.t>,
    has: Item.t => bool,
    isOnly: Item.t => bool,
    isMulti: unit => bool,
    isEmpty: unit => bool,
    selectOne: Item.t => unit,
    deselectOne: Item.t => unit,
    concat: array<Item.t> => unit,
    fromArray: array<Item.t> => unit,
    restorePrevious: unit => unit,
    clear: unit => unit,
  }

  type state = {
    current: Belt.Set.t<Item.t, ComparableItem.identity>,
    previous: Belt.Set.t<Item.t, ComparableItem.identity>,
  }

  type action =
    | SelectOne(Item.t)
    | DeselectOne(Item.t)
    | Concat(array<Item.t>)
    | FromArray(array<Item.t>)
    | RestorePrevious
    | Clear

  let useSelection = (~topMarginFactor=3., ~bottomMarginFactor=3., ()) => {
    let initialState = React.useMemo0(() => {
      let set = Belt.Set.make(~id=module(ComparableItem))
      {current: set, previous: set}
    })

    let refs: React.ref<Belt.Map.t<Item.t, Dom.element, ComparableItem.identity>> = React.useRef(
      Belt.Map.make(~id=module(ComparableItem)),
    )

    let (state, dispatch) = ReactHooks.useReducer(initialState, (state, action) =>
      @log
      switch action {
      | SelectOne(id) =>
        UpdateWithSideEffects(
          {
            current: Belt.Set.make(~id=module(ComparableItem))->Belt.Set.add(id),
            previous: state.current,
          },
          _ =>
            refs.current
            ->Belt.Map.get(id)
            ->Option.mapOr((), Scroll.adjust(~topMarginFactor, ~bottomMarginFactor, ...)),
        )

      | DeselectOne(id) =>
        Update({
          current: state.current->Belt.Set.remove(id),
          previous: state.current,
        })

      | Concat(items) =>
        Update({
          current: state.current->Belt.Set.mergeMany(items),
          previous: state.current,
        })

      | FromArray(items) =>
        Update({
          current: items->Belt.Set.fromArray(~id=module(ComparableItem)),
          previous: state.current,
        })

      | RestorePrevious => Update({current: state.previous, previous: state.current})

      | Clear =>
        Update({
          current: Belt.Set.make(~id=module(ComparableItem)),
          previous: state.current,
        })
      }
    )

    {
      size: () => state.current->Belt.Set.size,
      array: () => state.current->Belt.Set.toArray,
      has: itemId => state.current->Belt.Set.has(itemId),
      isOnly: itemId => state.current->Belt.Set.size == 1 && state.current->Belt.Set.has(itemId),
      isMulti: () => state.current->Belt.Set.size > 1,
      isEmpty: () => state.current->Belt.Set.isEmpty,
      selectOne: itemId => SelectOne(itemId)->dispatch,
      deselectOne: itemId => DeselectOne(itemId)->dispatch,
      concat: items => Concat(items)->dispatch,
      fromArray: items => FromArray(items)->dispatch,
      restorePrevious: () => RestorePrevious->dispatch,
      clear: () => Clear->dispatch,
      register: (itemId, el) =>
        switch el->Nullable.toOption->Option.flatMap(Nullable.toOption) {
        | Some(el) => refs.current = refs.current->Belt.Map.set(itemId, el)
        | None => ()
        },
      dispose: itemId => {
        refs.current = refs.current->Belt.Map.remove(itemId)
        if state.current->Belt.Set.has(itemId) {
          DeselectOne(itemId)->dispatch
        }
      },
    }
  }
}
