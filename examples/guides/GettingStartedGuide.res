type item = int

module Item = {
  type t = item
  let eq: (item, item) => bool = (x1, x2) => x1 == x2
  let cmp: (item, item) => int = Pervasives.compare
}

module Container = Dnd.MakeSingletonContainer()

module Items = Dnd.Make(Item, Container)

type state = array<item>

type action = ReorderItems(Dnd.result<Item.t, Container.t>)

let reducer = (state, action) =>
  switch action {
  | ReorderItems(Some(SameContainer(item, placement))) =>
    state->ArrayExt.reinsert(
      ~value=item,
      ~place=switch placement {
      | Before(id) => #Before(id)
      | Last => #Last
      },
    )
  | ReorderItems(Some(NewContainer(_)))
  | ReorderItems(None) => state
  }

let initialState = [1, 2, 3, 4, 5, 6, 7]

@react.component
let make = () => {
  let (state, dispatch) = reducer->React.useReducer(initialState)

  <Items.DndManager onReorder={result => ReorderItems(result)->dispatch}>
    <Items.DroppableContainer id={Container.id()} axis=Y>
      {state
      ->Array.mapWithIndex((index, item) =>
        <Items.DraggableItem id=item key={item->Int.toString} containerId={Container.id()} index>
          #Children(item->Int.toString->React.string)
        </Items.DraggableItem>
      )
      ->React.array}
    </Items.DroppableContainer>
  </Items.DndManager>
}
