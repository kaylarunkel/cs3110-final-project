type risk_profile =
  | Safe
  | Average
  | Risky

let get_pie_data (amounts : (string * float) list) =
  let total_sum =
    List.fold_left (fun acc (_, amount) -> acc +. amount) 0.0 amounts
  in
  List.map (fun (_, amount) -> amount /. total_sum *. 100.) amounts

let required_savings_per_year age risk_profile retirement_goal current_savings
    expenditure_categories =
  let annual_return =
    match risk_profile with
    | Safe -> 0.05
    | Average -> 0.08
    | Risky -> 0.10
  in
  let years_until_retirement = 65 - age in
  let future_value =
    current_savings
    *. ((1. +. annual_return) ** float_of_int years_until_retirement)
  in
  let savings_needed = retirement_goal -. future_value in
  let yearly_savings_needed =
    savings_needed /. float_of_int years_until_retirement
  in
  let total_expenditure_percentage =
    List.fold_left
      (fun acc (_, percentage) -> acc +. percentage)
      0.0 expenditure_categories
  in
  let category_savings_needed =
    List.map
      (fun (category, percentage) ->
        ( category,
          percentage *. yearly_savings_needed /. total_expenditure_percentage ))
      expenditure_categories
  in
  category_savings_needed
