include Dnd__Types
include Dnd__Config

type result<'itemId, 'containerId> = option<ReorderResult.t<'itemId, 'containerId>>

module Make = (Item: DndEntry, Container: DndEntry) => {
  module DndContext = Dnd__DndContext.Make(Item, Container)
  module DndManager = Dnd__DndManager.Make(DndContext)
  module DraggableItem = Dnd__DraggableItem.Make(DndContext)
  module DroppableContainer = Dnd__DroppableContainer.Make(DndContext)
}

module MakeSingletonContainer = () => {
  type t
  external id: unit => t = "%identity"
  let eq = (_, _) => true
  let cmp = (_, _) => 0
}

module Selection = {
  module Make = Dnd__SelectionManager.Make
}
