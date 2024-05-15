open OUnit
open Expense_tracker.Expenses
open Expense_tracker.Pie
open Expense_tracker.Textbox

let expense0 =
  {
    description = "Target";
    category = "Clothing";
    amount = 20.0;
    date = "04/13/2024";
  }

let expense1 =
  {
    description = "Dinner";
    category = "Food";
    amount = 20.0;
    date = "04/13/2024";
  }

let expense2 =
  {
    description = "Board games";
    category = "Entertainment";
    amount = 125.99;
    date = "02/12/2023";
  }

let expense3 =
  {
    description = "Breakfast with friends";
    category = "Food";
    amount = 54.50;
    date = "04/21/2024";
  }

let expense4 =
  {
    description = "Taco Bell";
    category = "Food";
    amount = 40.0;
    date = "03/23/2024";
  }

let expense5 =
  {
    description = "SAT";
    category = "Education";
    amount = 160.0;
    date = "06/27/2024";
  }

let expenses_tests =
  "test suite for expenses"
  >::: [
         ( "check expenses" >:: fun _ ->
           assert_equal "Breakfast with friends" expense3.description );
         ("check expenses" >:: fun _ -> assert_equal "Food" expense3.category);
         ("check expenses" >:: fun _ -> assert_equal 54.50 expense3.amount);
         ("check expenses" >:: fun _ -> assert_equal "04/21/2024" expense3.date);
         ( "check expenses" >:: fun _ ->
           assert_equal "Board games" expense2.description );
         ( "check expenses" >:: fun _ ->
           assert_equal "Entertainment" expense2.category );
         ("check expenses" >:: fun _ -> assert_equal 125.99 expense2.amount);
         ("check expenses" >:: fun _ -> assert_equal "02/12/2023" expense2.date);
         ( "add expense to empty list" >:: fun _ ->
           assert_equal [ expense1 ]
             (add_expense [] "Dinner" "Food" 20.0 "04/13/2024") );
         ( "add expense to expense list" >:: fun _ ->
           assert_equal [ expense2; expense1 ]
             (add_expense [ expense1 ] "Board games" "Entertainment" 125.99
                "02/12/2023") );
         ( "total expenses when list is empty" >:: fun _ ->
           assert_equal 0.0 (total_expenses []) );
         ( "total expense with one expense" >:: fun _ ->
           assert_equal expense1.amount (total_expenses [ expense1 ]) );
         ( "total expense with multiple expenses" >:: fun _ ->
           assert_equal
             (expense1.amount +. expense2.amount)
             (total_expenses [ expense1; expense2 ]) );
         ( "read from csv" >:: fun _ ->
           assert_equal [ expense1; expense2 ]
             (read_expenses_from_csv "testexpenses.csv") );
         ( "get food expenses" >:: fun _ ->
           assert_equal [ expense1 ]
             (get_expenses [ expense1; expense2 ] "Food") );
         ( "get expenses by date" >:: fun _ ->
           assert_equal [ expense1 ]
             (get_expenses [ expense2; expense1 ] "04/13/2024") );
         ( "get no expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "01/02/2023") );
         ( "get categories" >:: fun _ ->
           assert_equal
             [ "Entertainment"; "Food" ]
             (get_categories [ expense1; expense2 ]) );
         ( "get amount per category" >:: fun _ ->
           assert_equal
             [ ("Entertainment", 125.99); ("Food", 20.0) ]
             (amount_by_category [ expense1; expense2 ]
                (get_categories [ expense1; expense2 ])) );
         ( "get amount per category with repeat categories" >:: fun _ ->
           assert_equal
             [ ("Entertainment", 125.99); ("Food", 74.50) ]
             (amount_by_category
                [ expense1; expense2; expense3 ]
                (get_categories [ expense1; expense2; expense3 ])) );
         ("get year" >:: fun _ -> assert_equal "2024" (get_year "04/23/2024"));
         ( "get year from invalid date format" >:: fun _ ->
           assert_raises (Failure "Invalid date format") (fun () ->
               get_year "2024/23/4") );
         ( "get expenses by year" >:: fun _ ->
           assert_equal
             [ ("2024", 20.0) ]
             (total_expenses_per_year [ expense0 ]) );
         ( "get expenses by year" >:: fun _ ->
           assert_equal
             [ ("2023", 125.99); ("2024", 20.0) ]
             (total_expenses_per_year [ expense0; expense2 ]) );
         ( "get expenses by year" >:: fun _ ->
           assert_equal
             [ ("2023", 125.99); ("2024", 20.0) ]
             (total_expenses_per_year [ expense2; expense0 ]) );
         ( "get expenses by year" >:: fun _ ->
           assert_equal
             [ ("2023", 125.99); ("2024", 74.50) ]
             (total_expenses_per_year [ expense2; expense0; expense3 ]) );
         ( "format money string" >:: fun _ ->
           assert_equal "3.00" (money_string "3.") );
         ( "format money string" >:: fun _ ->
           assert_equal "12.00" (money_string "12.") );
         ( "format money string" >:: fun _ ->
           assert_equal "14.50" (money_string "14.5") );
         ( "format money string" >:: fun _ ->
           assert_equal "120.00" (money_string "120.0") );
         ( "format money string" >:: fun _ ->
           assert_equal "0.00" (money_string "0.") );
         ( "format money string" >:: fun _ ->
           assert_equal "0.50" (money_string "0.50") );
         ("get years from expenses" >:: fun _ -> assert_equal [] []);
         ( "get years from expenses" >:: fun _ ->
           assert_equal (possible_years_list [ expense0 ]) [ 2024 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal (possible_years_list [ expense0; expense1 ]) [ 2024 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal
             (possible_years_list [ expense0; expense2 ])
             [ 2024; 2023 ] );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [] (get_expense_by_year [ expense0 ] "2022") );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0 ] (get_expense_by_year [ expense0 ] "2024")
         );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (get_expense_by_year [ expense0; expense1 ] "2024") );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0 ]
             (get_expense_by_year [ expense0; expense2 ] "2024") );
         ( "sort expenses by year" >:: fun _ ->
           assert_equal [ ("2022", 30.0) ] (sorted_by_year [ ("2022", 30.0) ])
         );
         ( "sort expenses by year" >:: fun _ ->
           assert_equal
             [ ("2022", 30.0); ("2023", 45.5) ]
             (sorted_by_year [ ("2022", 30.0); ("2023", 45.5) ]) );
         ( "sort expenses by year" >:: fun _ ->
           assert_equal
             [ ("2022", 30.0); ("2023", 45.5) ]
             (sorted_by_year [ ("2023", 45.5); ("2022", 30.0) ]) );
         ( "expenses by date" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0 ] "03/12/2023" "03/15/2023") );
         ( "expenses by date" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_by_date_range [ expense0 ] "03/12/2023" "05/15/2024") );
         ( "expenses by date" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_by_date_range [ expense0; expense1 ] "03/12/2023"
                "05/15/2024") );
         ( "expenses by date" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_by_date_range [ expense0; expense1 ] "03/12/2023"
                "04/13/2024") );
         ( "expenses by date" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_by_date_range [ expense0; expense1 ] "04/13/2024"
                "04/14/2024") );
         ( "expenses by date" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_by_date_range [ expense0; expense2 ] "04/13/2023"
                "04/14/2024") );
         ( "expenses by date" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0; expense2 ] "02/13/2021"
                "01/14/2022") );
         ( "expensess above amount value" >:: fun _ ->
           assert_equal [] (expenses_above [ expense0 ] 10000.) );
         ( "expensess above amount value" >:: fun _ ->
           assert_equal [ expense0 ] (expenses_above [ expense0 ] 20.) );
         ( "expensess above amount value" >:: fun _ ->
           assert_equal [ expense0 ] (expenses_above [ expense0 ] 1.) );
         ( "expensess above amount value" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_above [ expense0; expense1 ] 1.) );
         ( "expensess above amount value" >:: fun _ ->
           assert_equal [ expense2 ]
             (expenses_above [ expense0; expense2 ] 100.) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [] (expenses_below [ expense0 ] 19.) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [ expense0 ] (expenses_below [ expense0 ] 21.) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [ expense0 ] (expenses_below [ expense0 ] 20.01) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_below [ expense0; expense1 ] 20.01) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_below [ expense0; expense2 ] 20.01) );
         ( "expenses below amount value" >:: fun _ ->
           assert_equal [ expense0; expense2 ]
             (expenses_below [ expense0; expense2 ] 130.59) );
         ( "expenses between amount values" >:: fun _ ->
           assert_equal [] (expenses_between_ammounts [ expense0 ] 30.0 40.0) );
         ( "expenses between amount values" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_between_ammounts [ expense0 ] 10.0 40.0) );
         ( "expenses between amount values" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_between_ammounts [ expense0; expense1 ] 10.0 40.0) );
         ( "expenses between amount values" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_between_ammounts [ expense0; expense2 ] 10.0 40.0) );
         ( "expenses between amount values" >:: fun _ ->
           assert_equal [ expense0; expense2 ]
             (expenses_between_ammounts [ expense0; expense2 ] 10.0 400.0) );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Clothing", 100.) ]
             (percentage_of_total_expenses_by_category [ expense0 ]) );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Food", 50.); ("Clothing", 50.) ]
             (percentage_of_total_expenses_by_category [ expense0; expense1 ])
         );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Food", 75.); ("Clothing", 25.) ]
             (percentage_of_total_expenses_by_category
                [ expense0; expense1; expense4 ]) );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Education", 100.) ]
             (percentage_of_total_expenses_by_category [ expense5 ]) );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Food", 20.); ("Education", 80.) ]
             (percentage_of_total_expenses_by_category [ expense5; expense4 ])
         );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Food", 20.); ("Education", 80.) ]
             (percentage_of_total_expenses_by_category [ expense5; expense4 ])
         );
         ( "percentage of expenses per category" >:: fun _ ->
           assert_equal
             [ ("Education", 100.) ]
             (percentage_of_total_expenses_by_category [ expense5; expense2 ])
         );
       ]

let pie_tests =
  "test suite for pies"
  >::: [
         ( "floats to percentages" >:: fun _ ->
           assert_equal [ 50.0; 50.0 ]
             (get_pie_data
                (amount_by_category [ expense1; expense0 ]
                   (get_categories [ expense1; expense0 ]))) );
       ]

let textbox_tests =
  "test suite for textbox"
  >::: [
         ( "create textbox" >:: fun _ ->
           assert_equal { content = ""; cursor_pos = 0 } (create_textbox ()) );
       ]

let budget_tests =
  "test suite for budget algorithm"
  >::: [
         ( "calculate present value for retirement" >:: fun _ ->
           assert_equal 0. (present_value_retirement_func 0. 0. 0) );
         ( "calculate present value for retirement" >:: fun _ ->
           assert_equal 25. (present_value_retirement_func 100. 1. 2) );
         ( "calculate present value for retirement" >:: fun _ ->
           assert_equal 1. (present_value_retirement_func 1024. 3. 5) );
       ]

let _ = run_test_tt_main expenses_tests
let _ = run_test_tt_main pie_tests
let _ = run_test_tt_main textbox_tests
let _ = run_test_tt_main budget_tests
