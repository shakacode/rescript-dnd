module Int = {
  let toString = Js.Int.toString;
};

module Float = {
  external fromInt: int => float = "%floatofint";
  let toString = Js.Float.toString;
  let fromString = Js.Float.fromString;
  let toSortFactor = x => x > 0. ? 1 : x < 0. ? (-1) : 0;
};
