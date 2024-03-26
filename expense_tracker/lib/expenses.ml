type expense = {
  description : string;
  category : string;
  amount : float;
}

type expense_list = expense list

let add_expense (list : expense_list) (description : string) (category : string)
    (amount : float) : expense_list =
  { description; category; amount } :: list

let view_expenses (list : expense_list) : unit =
  List.iter
    (fun exp ->
      Printf.printf "Description: %s, Category: %s, Amount: %.2f\n"
        exp.description exp.category exp.amount)
    list

let total_expenses (list : expense_list) : float =
  List.fold_left (fun acc exp -> acc +. exp.amount) 0.0 list

let read_expenses_from_csv (filename : string) : expense_list =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      match String.split_on_char ',' line with
      | [ description; category; amount_str ] ->
          let amount = float_of_string amount_str in
          read_lines ({ description; category; amount } :: acc)
      | _ -> read_lines acc
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let save_expenses_to_csv (filename : string) (list : expense_list) : unit =
  let oc = open_out filename in
  List.iter
    (fun exp -> Printf.fprintf oc "%s,%.2f\n" exp.description exp.amount)
    list;
  close_out oc
