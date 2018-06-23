module type Config = {
  module Draggable: {type t; let eq: (t, t) => bool;};
  module Droppable: {type t; let eq: (t, t) => bool;};
};
