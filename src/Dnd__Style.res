module Web = Dnd__Web

let px = x => x->Float.toString ++ "px"
let stripPx = x => x->Js.String.replace("px", "", _)->Float.fromString->Option.getExn

let animationFunction = "cubic-bezier(0.2, 0, 0, 1)"
let animationDuration = 200
let resetAnimationsFactor = -70
let finishDropFactor = 20

let transition = prop =>
  prop ++ (" " ++ (animationDuration->Int.toString ++ ("ms " ++ animationFunction)))

let translate = (x, y) => "translate(" ++ (x->px ++ (", " ++ (y->px ++ ")")))

let getComputedStyle = element => {
  open Webapi.Dom
  window->Window.getComputedStyle(element->Web.htmlElementToElement)
}
