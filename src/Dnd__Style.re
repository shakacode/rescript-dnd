let px = n => (n |. string_of_int) ++ "px";
let stripPx = v => v |> Js.String.replace("px", "") |> int_of_string;

let animationFunction = "cubic-bezier(0.2, 0, 0, 1)";
let animationDuration = 200;
let resetAnimationsFactor = (-70);
let finishDropFactor = 20;

let transition = prop =>
  prop
  ++ " "
  ++ (animationDuration |. string_of_int)
  ++ "ms "
  ++ animationFunction;

let translate = (x, y) =>
  "translate(" ++ (x |> px) ++ ", " ++ (y |> px) ++ ")";
