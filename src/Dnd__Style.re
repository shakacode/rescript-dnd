module Html = Dnd__Html;

let px = n => (n |. string_of_int) ++ "px";
let stripPx = v =>
  v |> Js.String.replace("px", "") |> float_of_string |> int_of_float;

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

let getComputedStyle = element =>
  Webapi.Dom.(
    window
    |> Window.getComputedStyle(element |> Html.castHtmlElementToElement)
  );
