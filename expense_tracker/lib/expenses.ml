type expense = {
  description : string;
  category : string;
  amount : float;
  date : string;
}

type risk_profile =
  | Safe
  | Average
  | Risky

type expense_list = expense list

let add_expense (list : expense_list) (description : string) (category : string)
    (amount : float) (date : string) : expense_list =
  { description; category; amount; date } :: list

let view_expenses (list : expense_list) : unit =
  List.iter
    (fun exp ->
      Printf.printf "Description: %s, Category: %s, Amount: %.2f, Date: %s\n"
        exp.description exp.category exp.amount exp.date)
    list

let total_expenses (list : expense_list) : float =
  List.fold_left (fun acc exp -> acc +. exp.amount) 0.0 list

let read_expenses_from_csv (filename : string) : expense_list =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        match String.split_on_char ',' line with
        | [ description; category; amount_str; date ] ->
            let amount = float_of_string amount_str in
            read_lines ({ description; category; amount; date } :: acc)
        | _ -> read_lines acc
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines []
  with Sys_error _ ->
    Printf.printf "The CSV file '%s' doesn't exist.\n" filename;
    []

let save_expenses_to_csv (filename : string) (list : expense_list) : unit =
  let oc = open_out filename in
  List.iter
    (fun exp ->
      Printf.fprintf oc "%s, %s, %.2f, %s\n" exp.description exp.category
        exp.amount exp.date)
    list;
  close_out oc

let get_expenses (list : expense_list) (criteria : string) : expense_list =
  List.filter (fun exp -> exp.category = criteria || exp.date = criteria) list

let get_categories (expenses : expense_list) : string list =
  let rec collect_categories seen expenses =
    match expenses with
    | [] -> seen
    | { category; _ } :: rest ->
        if List.mem category seen then collect_categories seen rest
        else collect_categories (category :: seen) rest
  in
  collect_categories [] expenses

let amount_by_category (expenses : expense_list) (categories : string list) =
  let initial_totals = List.map (fun _ -> 0.0) categories in
  let category_with_amount = List.combine categories initial_totals in
  let update_totals total_to_update expense =
    let amount =
      List.assoc expense.category total_to_update +. expense.amount
    in
    List.map
      (fun (category, category_amount) ->
        if category = expense.category then (category, amount)
        else (category, category_amount))
      total_to_update
  in
  List.fold_left update_totals category_with_amount expenses

let assert_for_expenses_by_date_range start_date end_date =
  let assert_date_format date =
    assert (String.length date = 10 && date.[2] = '/' && date.[5] = '/')
  in
  assert_date_format start_date;
  assert_date_format end_date

let parse_date date_str =
  try
    let parts = String.split_on_char '/' (String.trim date_str) in
    let year = int_of_string (List.nth parts 2) in
    let month = int_of_string (List.nth parts 0) in
    let day = int_of_string (List.nth parts 1) in
    (year, month, day)
  with Failure _ as e ->
    Printf.printf "Failed to parse date string: %s\n" date_str;
    raise e

let is_in_range start_date end_date expense =
  let start_year, start_month, start_day = parse_date start_date in
  let end_year, end_month, end_day = parse_date end_date in
  let expense_year, expense_month, expense_day = parse_date expense.date in
  let start_date = (start_year * 10000) + (start_month * 100) + start_day in
  let end_date = (end_year * 10000) + (end_month * 100) + end_day in
  let expense_date =
    (expense_year * 10000) + (expense_month * 100) + expense_day
  in
  start_date <= expense_date && expense_date <= end_date

let expenses_by_date_range (expenses : expense list) (start_date : string)
    (end_date : string) : expense list =
  assert_for_expenses_by_date_range start_date end_date;
  List.filter (is_in_range start_date end_date) expenses

let expenses_above (expenses : expense list) (floor : float) =
  List.filter (fun x -> x.amount >= floor) expenses

let expenses_below (expenses : expense list) (ceiling : float) =
  List.filter (fun x -> x.amount <= ceiling) expenses

let expenses_between_ammounts (expenses : expense list) (floor : float)
    (ceiling : float) =
  List.filter (fun x -> floor <= x.amount && ceiling >= x.amount) expenses

let get_year (date : string) : string =
  let date = String.trim date in
  if String.length date <> 10 || date.[2] <> '/' || date.[5] <> '/' then
    failwith "Invalid date format";
  let parts = String.split_on_char '/' date in
  match parts with
  | [ _; _; year ] -> year
  | _ -> failwith "Invalid date format"

let sorted_by_year (years_with_amounts : (string * float) list) =
  List.sort
    (fun (year1, _) (year2, _) -> int_of_string year1 - int_of_string year2)
    years_with_amounts

let total_expenses_per_year (expenses : expense_list) : (string * float) list =
  let rec group_by_year acc = function
    | [] -> acc
    | expense :: rest ->
        let year = get_year expense.date in
        let total =
          match List.assoc_opt year acc with
          | Some total -> total +. expense.amount
          | None -> expense.amount
        in
        let updated_acc = (year, total) :: List.remove_assoc year acc in
        group_by_year updated_acc rest
  in
  sorted_by_year (group_by_year [] expenses)

let get_expense_by_year (list : expense_list) (year : string) : expense list =
  let get_year_from_date date =
    match String.split_on_char '/' date with
    | [ _; _; year ] -> year
    | _ -> failwith "invalid date"
  in
  List.filter (fun expense -> get_year_from_date expense.date = year) list

let possible_years (list : expense_list) =
  let lst = total_expenses_per_year list in
  let rec concat acc lst =
    match lst with
    | [] -> acc
    | (h, _) :: [] -> acc ^ h
    | (h, _) :: t -> concat (acc ^ h ^ ", ") t
  in
  concat "" lst

let possible_years_list (list : expense_list) =
  let lst = total_expenses_per_year list in
  let rec recreate acc lst =
    match lst with
    | [] -> acc
    | (h, _) :: t -> recreate (int_of_string h :: acc) t
  in
  recreate [] lst

let money_string amount =
  let num = String.index_from amount 0 '.' in
  let len = String.length amount in
  if num + 1 = len then amount ^ "00"
  else if num + 2 = len then amount ^ "0"
  else amount

let exp_category_refactored expenses_recent_year =
  amount_by_category expenses_recent_year (get_categories expenses_recent_year)

let percentage_of_total_expenses_by_category expenses =
  let recent_year = List.hd (possible_years_list expenses) |> string_of_int in
  let expenses_recent_year = get_expense_by_year expenses recent_year in
  let total_expenses_recent_year = total_expenses expenses_recent_year in
  let expenses_by_category = exp_category_refactored expenses_recent_year in
  List.map
    (fun (category, amount) ->
      (category, amount /. total_expenses_recent_year *. 100.0))
    expenses_by_category

let retirement_func future_value discount_rate years =
  future_value /. ((1.0 +. discount_rate) ** float_of_int years)

let money_needed age risk goal bank =
  let growth_rate =
    match risk with
    | Safe -> 0.04
    | Average -> 0.06
    | Risky -> 0.1
  in
  let years_left = 65 - age in
  let present_value_retirement = retirement_func goal 0.05 years_left in
  let future_value_savings =
    bank *. (1. +. (growth_rate ** float_of_int years_left))
  in
  let present_value_savings =
    future_value_savings /. (1.05 ** float_of_int years_left)
  in
  let present_money_needed =
    present_value_retirement -. present_value_savings
  in
  present_money_needed *. (0.05 -. growth_rate)
  /. (1. -. (((1. +. growth_rate) /. 1.05) ** float_of_int years_left))

let young_age_savings age risk budget income goal bank =
  let recent_year = List.hd (possible_years_list budget) |> string_of_int in
  let budget = get_expense_by_year budget recent_year in
  if income < total_expenses budget then
    "You are spending too much relative to your income. We suggest youreview \
     your expense breakdown. \n\
    \ Check your piechart for more information."
  else
    let money_per_year = money_needed age risk goal bank in
    let current_savings_per_year = income -. total_expenses budget in
    let difference = money_per_year -. current_savings_per_year in
    let percent_change = difference /. total_expenses budget *. 100. in
    if Float.round percent_change > 0. then
      "You have to cut your budget by "
      ^ string_of_float (Float.round percent_change)
      ^ "%"
    else if Float.round percent_change < 0. then
      "You can raise your expenditure by "
      ^ string_of_float (-1. *. Float.round percent_change)
      ^ "%"
    else "You don't have to change a thing! You are on the right track."

let required_savings_per_year age risk_profile budget income retirement_goal
    bank_balance =
  if age >= 65 then
    "You're too old partner! There's no point in tracking your budget now."
  else
    let recent_year = List.hd (possible_years_list budget) |> string_of_int in
    let budget = get_expense_by_year budget recent_year in
    if income < total_expenses budget then
      "You are spending too much relative to your income. We suggest youreview \
       your expense breakdown. \n\
      \ Check your piechart for more information."
    else
      young_age_savings age risk_profile budget income retirement_goal
        bank_balance
