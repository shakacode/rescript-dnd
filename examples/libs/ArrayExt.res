let findIndexOf = (arr: array<'a>, x: 'a) =>
  switch arr->Array.findIndex(x' => x' === x) {
  | -1 => failwith(`Unable to find \`$(x)\` in array \`$(arr)\``)
  | _ as i => i
  }

let insert = (arr: array<'a>, ~value: 'a, ~place: [#Before('a) | #Last]) => {
  let arr = arr->Array.copy
  arr
  ->Array.splice(
    ~start=switch place {
    | #Before(x) => arr->findIndexOf(x)
    | #Last => arr->Array.length
    },
    ~remove=0,
    ~insert=[value],
  )
  ->ignore
  arr
}

let reinsert = (arr: array<'a>, ~value: 'a, ~place: [#Before('a) | #Last]) => {
  let arr = arr->Array.copy
  let from = arr->findIndexOf(value)
  arr->Array.splice(~start=from, ~remove=1, ~insert=[])->ignore
  arr
  ->Array.splice(
    ~start=switch place {
    | #Before(x) => arr->findIndexOf(x)
    | #Last => arr->Array.length
    },
    ~remove=0,
    ~insert=[value],
  )
  ->ignore
  arr
}
