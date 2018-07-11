let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <ItemsAmountContainer startWith=50>
      ...(
           (~amount, ~updateAmount) =>
             <div className="example-wrapper">
               <div className="example-header">
                 <div className="example-header-title">
                   ("Scrollable container" |> ReasonReact.string)
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
               <div className="scrollable-container">
                 <VerticalListContainer
                   key=(amount |> string_of_int)
                   n=amount
                 />
               </div>
             </div>
         )
    </ItemsAmountContainer>,
};
