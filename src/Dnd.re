open Dnd__Types;

type result('draggableId, 'droppableId) =
  DropResult.t('draggableId, 'droppableId);

module Make = (Config: Dnd__Config.Config) => {
  module Context = Dnd__Context.Make(Config);
  module Draggable = Dnd__Draggable.Make(Config);
  module Droppable = Dnd__Droppable.Make(Config);
};
