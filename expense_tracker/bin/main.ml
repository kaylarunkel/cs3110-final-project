open Expense_tracker.Expenses
open Expense_tracker.Pie

let rec main (list : expense_list) : unit =
  Printf.printf "\nExpense Tracker\n";
  Printf.printf "1. Add Expense\n";
  Printf.printf "2. View Expenses\n";
  Printf.printf "3. Total Expenses\n";
  Printf.printf "4. Read Expenses from CSV\n";
  Printf.printf "5. Export Expenses to CSV\n";
  Printf.printf "6. Analyze My Expenses\n";
  Printf.printf "7. Exit\n";
  Printf.printf "Choose an option: ";

  match read_int () with
  | 1 ->
      Printf.printf "Enter description: ";
      let description = read_line () in
      Printf.printf "Enter category: ";
      let category = read_line () in
      Printf.printf "Enter amount: ";
      let amount = read_float () in
      Printf.printf "Enter date: ";
      let date = read_line () in
      let new_list = add_expense list description category amount date in
      main new_list
  | 2 ->
      view_expenses list;
      main list
  | 3 ->
      Printf.printf "Total Expenses: %.2f\n" (total_expenses list);
      main list
  | 4 ->
      Printf.printf "Enter the CSV file name: ";
      let input = read_line () in
      let filename = input in
      let new_list = read_expenses_from_csv filename in
      Printf.printf "Expenses read from CSV file.\n";
      main new_list
  | 5 ->
      Printf.printf "Enter desired file name (include .csv ending please): ";
      let filename = read_line () in
      save_expenses_to_csv filename list;
      main list
  | 6 -> (
      Printf.printf "Choose how you would like to analyze your expenses: \n";
      Printf.printf "1. View Pie Chart of Expenses per Category\n";
      Printf.printf "Choose an option: ";
      match read_int () with
      | 1 ->
          let categories = get_categories list in
          let data = get_pie_data (amount_by_category list categories) in
          draw_pie_chart_with_labels data (Array.of_list categories);
          main list
      | _ ->
          Printf.printf "Invalid option\n";
          main list)
  | 7 -> Printf.printf "Exiting...\n"
  | _ ->
      Printf.printf "Invalid option\n";
      main list

let () = main []
