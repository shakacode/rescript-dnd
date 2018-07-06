open Webapi.Dom;
open Dnd__Types;

module Speed = {
  type t = {
    x: int,
    y: int,
  };

  let startFrom = 0.25;
  let maxSpeedAt = 0.05;
  let maxSpeed = 28;

  let calculate =
      (point: Point.t, dimensions: Dimensions.t, direction: Direction.t)
      : option(t) => {
    let height = dimensions.height |> float_of_int;
    switch (direction) {
    | Alpha =>
      let startFrom = height *. startFrom |> int_of_float;
      let maxSpeedAt = height *. maxSpeedAt |> int_of_float;
      if (point.y <= startFrom && point.y > maxSpeedAt) {
        let scrollRange = startFrom - maxSpeedAt |> float_of_int;
        let distanceFromStart = startFrom - point.y |> float_of_int;
        let powed =
          Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0);
        Some({
          x: 0,
          y: - ((maxSpeed |> float_of_int) *. powed |> int_of_float),
        });
      } else if (point.y <= maxSpeedAt) {
        Some({x: 0, y: - maxSpeed});
      } else {
        None;
      };

    | Omega =>
      let startFrom = height *. (1.0 -. startFrom) |> int_of_float;
      let maxSpeedAt = height *. (1.0 -. maxSpeedAt) |> int_of_float;
      if (point.y >= startFrom && point.y < maxSpeedAt) {
        let scrollRange = maxSpeedAt - startFrom |> float_of_int;
        let distanceFromStart = point.y - startFrom |> float_of_int;
        let powed =
          Js.Math.pow_float(~base=distanceFromStart /. scrollRange, ~exp=2.0);
        Some({x: 0, y: (maxSpeed |> float_of_int) *. powed |> int_of_float});
      } else if (point.y >= maxSpeedAt) {
        Some({x: 0, y: maxSpeed});
      } else {
        None;
      };
    };
  };
};

let scrollWindow =
    (
      point: RelativityBag.t(Point.t),
      scroll: Scroll.t,
      viewport: Dimensions.t,
      onScroll: unit => unit,
    ) => {
  let direction =
    Direction.(
      if (viewport.height / 2 > point.viewport.y) {
        Alpha;
      } else {
        Omega;
      }
    );

  switch (direction) {
  | Alpha =>
    let canScroll = scroll.current.y > 0;

    if (canScroll) {
      Speed.calculate(point.viewport, viewport, direction)
      |. Option.map(speed =>
           Webapi.requestCancellableAnimationFrame(_ => {
             window |> Window.scrollBy(speed.x, speed.y);
             onScroll();
           })
         );
    } else {
      None;
    };
  | Omega =>
    let canScroll = scroll.max.height !== scroll.current.y + viewport.height;

    if (canScroll) {
      Speed.calculate(point.viewport, viewport, direction)
      |. Option.map(speed =>
           Webapi.requestCancellableAnimationFrame(_ => {
             window |> Window.scrollBy(speed.x, speed.y);
             onScroll();
           })
         );
    } else {
      None;
    };
  };
};
