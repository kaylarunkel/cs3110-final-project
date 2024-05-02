open Expense_tracker.Expenses
open Expense_tracker.Pie
open Expense_tracker.Bar

let read_valid_string prompt =
  let rec loop () =
    Printf.printf "%s" prompt;
    let input = read_line () in
    if String.trim input = "" then begin
      Printf.printf "Please enter a non-empty string.\n";
      loop ()
    end
    else input
  in
  loop ()

let read_valid_float prompt =
  let rec loop () =
    Printf.printf "%s" prompt;
    try read_float ()
    with Failure _ ->
      Printf.printf "Please enter a valid floating-point number.\n";
      loop ()
  in
  loop ()

let read_valid_date prompt =
  let rec loop () =
    Printf.printf "%s" prompt;
    let input = read_line () in
    match String.split_on_char '/' input with
    | [ month; day; year ] -> begin
        try
          let month_int = int_of_string month in
          let day_int = int_of_string day in
          let year_int = int_of_string year in
          if month_int < 1 || month_int > 12 then begin
            Printf.printf "Invalid month. Please enter a valid month (1-12).\n";
            loop ()
          end
          else
            let max_days =
              match month_int with
              | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
              | 4 | 6 | 9 | 11 -> 30
              | 2 ->
                  if
                    year_int mod 4 = 0
                    && (year_int mod 100 <> 0 || year_int mod 400 = 0)
                  then 29
                  else 28
              | _ -> failwith "Invalid month"
            in
            if day_int < 1 || day_int > max_days then begin
              Printf.printf
                "Invalid day for the specified month. Please enter a valid day.\n";
              loop ()
            end
            else input
        with _ ->
          Printf.printf
            "Invalid date format or out-of-range date. Please enter a valid \
             date (MM/DD/YYYY).\n";
          loop ()
      end
    | _ ->
        Printf.printf
          "Invalid date format. Please enter a valid date (MM/DD/YYYY).\n";
        loop ()
  in
  loop ()

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
      let description = read_valid_string "Enter description: " in
      let category = read_valid_string "Enter category: " in
      let amount = read_valid_float "Enter amount: " in
      let date = read_valid_date "Enter date (MM/DD/YYYY): " in
      let new_list = add_expense list description category amount date in
      main new_list
  | 2 ->
      view_expenses list;
      main list
  | 3 ->
      Printf.printf
        "Choose in which format you would like to view your total expenses: \n";
      Printf.printf "1. Total for all expenses ever\n";
      Printf.printf "2. Total over a certain period of time\n";
      Printf.printf "Choose an option: ";
      begin
        match read_int () with
        | 1 ->
            Printf.printf "Total Expenses: %.2f\n" (total_expenses list);
            main list
        | 2 ->
            let start_date =
              read_valid_date "Enter start date (MM/DD/YYYY): "
            in
            let end_date = read_valid_date "Enter end date (MM/DD/YYYY): " in
            Printf.printf "Total Expenses for the specified range: %.2f\n"
              (total_expenses (expenses_by_date_range list start_date end_date));
            main list
        | _ ->
            Printf.printf "Invalid option\n";
            main list
      end
  | 4 ->
      let filename = read_valid_string "Enter the CSV file name: " in
      let new_list = read_expenses_from_csv filename in
      Printf.printf "Expenses read from CSV file.\n";
      main new_list
  | 5 ->
      let filename =
        read_valid_string
          "Enter desired file name (include .csv ending please): "
      in
      save_expenses_to_csv filename list;
      main list
  | 6 ->
      Printf.printf "Choose how you would like to analyze your expenses: \n";
      Printf.printf "1. View Pie Chart of Expenses per Category\n";
      Printf.printf "2. View Bar Graph of Yearly Expenses\n";
      Printf.printf "Choose an option: ";
      begin
        match read_int () with
        | 1 ->
            Printf.printf "Which year's expenses would you like to see: ";
            let year = read_line () in
            let specific_year_list = get_expense_by_year list year in
            let categories = get_categories specific_year_list in
            let data =
              get_pie_data (amount_by_category specific_year_list categories)
            in
            draw_pie_chart_with_labels data (Array.of_list categories);
            main list
        | 2 ->
            let yearly_amounts = total_expenses_per_year list in
            draw_bar_graph yearly_amounts;
            main list
        | _ ->
            Printf.printf "Invalid option\n";
            main list
      end
  | 7 -> Printf.printf "Exiting...\n"
  | _ ->
      Printf.printf "Invalid option\n";
      main list

let () = main []
