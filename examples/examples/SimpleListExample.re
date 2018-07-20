let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~layout, _) => {
  ...component,
  render: _ =>
    <ItemsAmountContainer startWith=7>
      ...(
           (~amount, ~updateAmount) =>
             <div className="example-wrapper">
               <div className="example-header">
                 <div className="example-header-title">
                   (
                     switch (layout) {
                     | Example.Vertical =>
                       "Vertical list" |> ReasonReact.string
                     | Example.Horizontal =>
                       "Horizontal list" |> ReasonReact.string
                     | Example.CardBoard =>
                       failwith("Don't use CardBoard layout with SimpleList")
                     }
                   )
                 </div>
                 <div className="example-header-toolbar">
                   <Label htmlFor="input-amount">
                     ("# of todos" |> ReasonReact.string)
                   </Label>
                   <Input
                     id="input-amount"
                     value=(
                       switch (amount) {
                       | 0 => ""
                       | _ as value => value |> string_of_int
                       }
                     )
                     onChange=updateAmount
                   />
                 </div>
               </div>
               <SimpleListContainer
                 key=(amount |> string_of_int)
                 n=amount
                 layout
               />
             </div>
         )
    </ItemsAmountContainer>,
};
