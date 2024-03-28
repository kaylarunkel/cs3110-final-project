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
      Printf.printf "Description: %s, Category: %s, Amount: %.2f\n"
        exp.description exp.category exp.amount)
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
    (fun exp -> Printf.fprintf oc "%s,%.2f\n" exp.description exp.amount)
    list;
  close_out oc

let get_expenses (list : expense_list) (criteria : string) : expense_list =
  List.filter (fun exp -> exp.category = criteria || exp.date = criteria) list
