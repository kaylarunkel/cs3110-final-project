type expense = {
  description : string;
  category : string;
  amount : float;
  date : string;
}

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

let expenses_by_date_range (expenses : expense list) (start_date : string)
    (end_date : string) : expense list =
  assert (start_date.[4] = '-');
  assert (start_date.[7] = '-');
  assert (end_date.[4] = '-');
  assert (end_date.[7] = '-');
  List.filter (fun x -> start_date <= x.date && end_date >= x.date) expenses

let expenses_above (expenses : expense list) (floor : float) =
  List.filter (fun x -> x.amount >= floor) expenses

let expenses_below (expenses : expense list) (ceiling : float) =
  List.filter (fun x -> x.amount <= ceiling) expenses

let expenses_between_ammounts (expenses : expense list) (floor : float)
    (ceiling : float) =
  List.filter (fun x -> floor <= x.amount && ceiling >= x.amount) expenses
