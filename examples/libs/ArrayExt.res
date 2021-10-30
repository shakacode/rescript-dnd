let findIndexOf = (arr: array<'a>, x: 'a) =>
  switch arr->Js.Array2.findIndex(x' => x' === x) {
  | -1 => failwith(j`Unable to find \`$(x)\` in array \`$(arr)\``)
  | _ as i => i
  }

let insert = (arr: array<'a>, ~value: 'a, ~place: [#Before('a) | #Last]) => {
  let arr = arr->Js.Array2.copy
  arr
  ->Js.Array2.spliceInPlace(
    ~pos=switch place {
    | #Before(x) => arr->findIndexOf(x)
    | #Last => arr->Js.Array.length
    },
    ~remove=0,
    ~add=[value],
  )
  ->ignore
  arr
}

let reinsert = (arr: array<'a>, ~value: 'a, ~place: [#Before('a) | #Last]) => {
  let arr = arr->Js.Array.copy
  let from = arr->findIndexOf(value)
  arr->Js.Array2.spliceInPlace(~pos=from, ~remove=1, ~add=[])->ignore
  arr
  ->Js.Array2.spliceInPlace(
    ~pos=switch place {
    | #Before(x) => arr->findIndexOf(x)
    | #Last => arr->Js.Array.length
    },
    ~remove=0,
    ~add=[value],
  )
  ->ignore
  arr
}
