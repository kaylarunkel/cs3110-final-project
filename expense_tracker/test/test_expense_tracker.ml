open OUnit
open Expense_tracker.Expenses

let expense1 =
  {
    description = "Dinner";
    category = "Food";
    amount = 20.0;
    date = "4/13/2024";
  }

let expense2 =
  {
    description = "Board games";
    category = "Entertainment";
    amount = 125.99;
    date = "2/12/2023";
  }

let expenses_tests =
  "test suite for expenses"
  >::: [
         ( "add expense to empty list" >:: fun _ ->
           assert_equal [ expense1 ]
             (add_expense [] "Dinner" "Food" 20.0 "4/13/2024") );
         ( "add expense to expense list" >:: fun _ ->
           assert_equal [ expense2; expense1 ]
             (add_expense [ expense1 ] "Board games" "Entertainment" 125.99
                "2/12/2023") );
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
             (get_expenses [ expense2; expense1 ] "4/13/2024") );
         ( "get no expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "1/2/2023") );
       ]

let _ = run_test_tt_main expenses_tests
