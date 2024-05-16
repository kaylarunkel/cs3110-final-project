open OUnit
open Expense_tracker.Expenses
open Expense_tracker.Pie

let expense0 =
  {
    description = "Target";
    category = "Clothing";
    amount = 20.0;
    date = "04/13/2024";
  }

let empty_expense = { description = ""; category = ""; amount = 0.0; date = "" }

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

let expense_list1 = [ expense0; expense1; expense2; expense3 ]

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

let expense6 =
  {
    description = "Netflix Subscription";
    category = "Entertainment";
    amount = 12.99;
    date = "04/15/2024";
  }

let expense7 =
  {
    description = "Gym Membership";
    category = "Health";
    amount = 50.0;
    date = "05/02/2024";
  }

let expense8 =
  {
    description = "Gasoline";
    category = "Transportation";
    amount = 35.0;
    date = "04/10/2024";
  }

let expense9 =
  {
    description = "Birthday Gift";
    category = "Gifts";
    amount = 25.0;
    date = "05/08/2024";
  }

let expense10 =
  {
    description = "Mobile Phone Bill";
    category = "Utilities";
    amount = 60.0;
    date = "05/20/2024";
  }

let expense_list2 = [ expense6; expense7; expense8; expense9; expense10 ]
let budget_list = [ expense0 ]

let expense11 =
  {
    description = "Movie Tickets";
    category = "Entertainment";
    amount = 20.0;
    date = "04/05/2024";
  }

let expense12 =
  {
    description = "Books";
    category = "Education";
    amount = 20.0;
    date = "05/12/2024";
  }

let expense13 =
  {
    description = "Coffee Beans";
    category = "Food";
    amount = 20.0;
    date = "04/20/2024";
  }

let expense14 =
  {
    description = "Medicine";
    category = "Health";
    amount = 20.0;
    date = "05/03/2024";
  }

let expense15 =
  {
    description = "Bus Fare";
    category = "Transportation";
    amount = 20.0;
    date = "04/17/2024";
  }

let expense16 =
  {
    description = "Gym Membership";
    category = "Fitness";
    amount = 20.0;
    date = "04/17/2022";
  }

let negative_expense =
  {
    description = "Gym Membership";
    category = "Fitness";
    amount = -20.0;
    date = "04/17/2022";
  }

let expenses_tests =
  "test suite for expenses"
  >::: [
         ( "check expenses" >:: fun _ ->
           assert_equal "Breakfast with friends" expense3.description );
         ( "check empty expense" >:: fun _ ->
           assert_equal "" empty_expense.description );
         ( "check empty expense" >:: fun _ ->
           assert_equal "" empty_expense.category );
         ( "check negative expense amount" >:: fun _ ->
           assert_equal (-20.0) negative_expense.amount );
         ("check empty expense" >:: fun _ -> assert_equal "" empty_expense.date);
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
         ( "add expense to expense list" >:: fun _ ->
           assert_equal [ empty_expense ] (add_expense [] "" "" 0.0 "") );
         ( "total expenses when list is empty" >:: fun _ ->
           assert_equal 0.0 (total_expenses []) );
         ( "total expense with one expense" >:: fun _ ->
           assert_equal expense1.amount (total_expenses [ expense1 ]) );
         ( "total expense with multiple expenses" >:: fun _ ->
           assert_equal
             (expense1.amount +. expense2.amount)
             (total_expenses [ expense1; expense2 ]) );
         ( "total expense with zero amount" >:: fun _ ->
           assert_equal 0.0 (total_expenses [ empty_expense ]) );
         ( "total expense with negative amount" >:: fun _ ->
           assert_equal (-20.0) (total_expenses [ negative_expense ]) );
         ( "read from csv" >:: fun _ ->
           assert_equal [ expense1; expense2 ]
             (read_expenses_from_csv "testexpenses.csv") );
         ( "get food expenses" >:: fun _ ->
           assert_equal [ expense1 ]
             (get_expenses [ expense1; expense2 ] "Food") );
         ( "get clothing expenses" >:: fun _ ->
           assert_equal [ expense0 ] (get_expenses [ expense0 ] "Clothing") );
         ( "get expenses by date" >:: fun _ ->
           assert_equal [ expense1 ]
             (get_expenses [ expense2; expense1 ] "04/13/2024") );
         ( "get no expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "01/02/2023") );
         ( "get empty expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ empty_expense ] " ") );
         ( "get no expenses_earlier" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "01/02/1900") );
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
         ( "format money string already done" >:: fun _ ->
           assert_equal "14.75" (money_string "14.75") );
         ( "format money string" >:: fun _ ->
           assert_equal "120.00" (money_string "120.0") );
         ( "format money string" >:: fun _ ->
           assert_equal "0.00" (money_string "0.") );
         ( "format money string" >:: fun _ ->
           assert_equal "0.50" (money_string "0.50") );
         ( "format money string" >:: fun _ ->
           assert_equal "1.50" (money_string "1.50") );
         ( "format negative string" >:: fun _ ->
           assert_equal "-1.50" (money_string "-1.50") );
         ( "format zero string" >:: fun _ ->
           assert_equal "-120.00" (money_string "-120.0") );
         ( "years as string" >:: fun _ ->
           assert_equal (possible_years [ expense0; expense1 ]) "2024" );
         ( "years as string" >:: fun _ ->
           assert_equal
             (possible_years [ expense0; expense1; expense2 ])
             "2023, 2024" );
         ( "years as string duplicate" >:: fun _ ->
           assert_equal
             (possible_years [ expense0; expense0; expense2 ])
             "2023, 2024" );
         ("get years from expenses" >:: fun _ -> assert_equal [] []);
         ( "get years from expenses" >:: fun _ ->
           assert_equal (possible_years_list [ expense0 ]) [ 2024 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal (possible_years_list [ expense0; expense1 ]) [ 2024 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal
             (possible_years_list [ expense0; expense1; expense2 ])
             [ 2024; 2023 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal (possible_years []) "" );
         ( "get years from expenses" >:: fun _ ->
           assert_equal
             (possible_years_list [ expense0; expense2 ])
             [ 2024; 2023 ] );
         ( "get years from expenses" >:: fun _ ->
           assert_equal
             (possible_years_list [ expense15; expense16 ])
             [ 2024; 2022 ] );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [] (get_expense_by_year [ expense0 ] "2022") );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0 ] (get_expense_by_year [ expense0 ] "2024")
         );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [] (get_expense_by_year [ expense0 ] "") );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (get_expense_by_year [ expense0; expense1 ] "2024") );
         ( "get expenses for specific year" >:: fun _ ->
           assert_equal [ expense0 ]
             (get_expense_by_year [ expense0; expense2 ] "2024") );
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
         ( "total expenses with zero amount" >:: fun _ ->
           assert_equal 0.0 (total_expenses [ { expense0 with amount = 0. } ])
         );
         ( "get non-existing category expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "Travel") );
         ( "get non-existing date expenses" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "01/01/2022") );
         ( "sort expenses by year with empty list" >:: fun _ ->
           assert_equal [] (sorted_by_year []) );
         ( "get expenses for non-existing year" >:: fun _ ->
           assert_equal [] (get_expense_by_year [ expense0; expense1 ] "2022")
         );
         ( "expenses by non-existing date range" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0; expense1 ] "01/01/2025"
                "01/01/2026") );
         ( "expenses by very-old date range" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0; expense1 ] "01/01/0000"
                "01/01/0001") );
         ( "expenses by very-wide date range" >:: fun _ ->
           assert_equal [ expense0; expense1 ]
             (expenses_by_date_range [ expense0; expense1 ] "01/01/0000"
                "01/01/3000") );
         ( "add expense with special characters in description" >:: fun _ ->
           let expense_special =
             {
               description = "Special @#&*";
               category = "Misc";
               amount = 15.0;
               date = "05/01/2024";
             }
           in
           assert_equal [ expense_special ]
             (add_expense [] "Special @#&*" "Misc" 15.0 "05/01/2024") );
         ( "add expense with special characters in description" >:: fun _ ->
           let expense_special =
             {
               description = "Defn 1";
               category = "Prac";
               amount = 150.75;
               date = "05/01/2000";
             }
           in
           assert_equal [ expense_special ]
             (add_expense [] "Defn 1" "Prac" 150.75 "05/01/2000") );
         ( "total expense with multiple expenses of different categories"
         >:: fun _ ->
           assert_equal
             (expense0.amount +. expense1.amount +. expense2.amount
            +. expense3.amount)
             (total_expenses [ expense0; expense1; expense2; expense3 ]) );
         ( "get expenses by category with empty list" >:: fun _ ->
           assert_equal [] (get_expenses [] "Food") );
         ( "get expenses by date with empty list" >:: fun _ ->
           assert_equal [] (get_expenses [] "01/01/2024") );
         ( "get categories from empty list" >:: fun _ ->
           assert_equal [] (get_categories []) );
         ( "get amount by category from empty list" >:: fun _ ->
           assert_equal [] (amount_by_category [] []) );
         ( "sort expenses by year with single entry" >:: fun _ ->
           assert_equal
             [ ("2024", expense0.amount) ]
             (sorted_by_year [ ("2024", expense0.amount) ]) );
         ( "expenses by single-date range with no matching entries" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range
                [ expense0; expense1; expense2 ]
                "01/01/2023" "01/01/2023") );
         ( "expenses by multiple-date range with no matching entries"
         >:: fun _ ->
           assert_equal []
             (expenses_by_date_range
                [ expense0; expense1; expense2 ]
                "01/01/1999" "01/01/2010") );
         ( "expenses above zero amount with empty list" >:: fun _ ->
           assert_equal [] (expenses_above [] 0.0) );
         ( "expenses below zero amount with empty list" >:: fun _ ->
           assert_equal [] (expenses_below [] 0.0) );
         ( "get expenses by category with no matching entries" >:: fun _ ->
           assert_equal [] (get_expenses [ expense1; expense2 ] "Travel") );
         ( "get categories from single-entry list" >:: fun _ ->
           assert_equal [ "Food" ] (get_categories [ expense1 ]) );
         ( "get amount by category from single-entry list" >:: fun _ ->
           assert_equal
             [ ("Food", 20.0) ]
             (amount_by_category [ expense1 ] [ "Food" ]) );
         ( "total expenses with multiple zero amount expenses" >:: fun _ ->
           assert_equal 0.0
             (total_expenses
                [ { expense0 with amount = 0. }; { expense1 with amount = 0. } ])
         );
         ( "get expenses by category with multiple matching entries" >:: fun _ ->
           assert_equal [ expense1; expense3 ]
             (get_expenses [ expense1; expense2; expense3 ] "Food") );
         ( "get expenses by category with multiple duplicate entries"
         >:: fun _ ->
           assert_equal
             [ expense1; expense1; expense1 ]
             (get_expenses [ expense1; expense1; expense1 ] "Food") );
         ( "get categories from multiple-entry list" >:: fun _ ->
           assert_equal
             [ "Entertainment"; "Food"; "Clothing" ]
             (get_categories [ expense0; expense1; expense2 ]) );
         ( "get amount by category from multiple-entry list" >:: fun _ ->
           assert_equal
             [ ("Food", 74.5); ("Entertainment", 125.99) ]
             (amount_by_category
                [ expense1; expense2; expense3 ]
                [ "Food"; "Entertainment" ]) );
         ( "expenses above zero amount with multiple entries" >:: fun _ ->
           assert_equal
             [ expense0; expense1; expense2; expense3 ]
             (expenses_above [ expense0; expense1; expense2; expense3 ] 0.0) );
         ( "expenses below max float amount with multiple entries" >:: fun _ ->
           assert_equal
             [ expense0; expense1; expense2; expense3 ]
             (expenses_below
                [ expense0; expense1; expense2; expense3 ]
                max_float) );
         ( "expenses below min float amount with multiple entries" >:: fun _ ->
           assert_equal []
             (expenses_below
                [ expense0; expense1; expense2; expense3 ]
                min_float) );
         ( "expenses between amount values with multiple entries" >:: fun _ ->
           assert_equal [ expense2; expense3 ]
             (expenses_between_ammounts
                [ expense0; expense1; expense2; expense3 ]
                30.0 200.0) );
         ( "expenses by date range with multiple entries" >:: fun _ ->
           assert_equal [ expense2 ]
             (expenses_by_date_range
                [ expense0; expense1; expense2; expense3 ]
                "01/01/2023" "12/31/2023") );
         ( "expenses by date range with single-entry list" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0 ] "01/01/2023" "12/31/2023") );
         ( "expenses by date range with no matching entries" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range
                [ expense0; expense1; expense2 ]
                "01/01/2025" "01/01/2026") );
         ( "total expenses with negative amount expenses" >:: fun _ ->
           assert_equal (-20.0)
             (total_expenses
                [
                  { expense0 with amount = -10. };
                  { expense1 with amount = -10. };
                ]) );
         ( "total expenses with offsetting amount expenses" >:: fun _ ->
           assert_equal 0.0
             (total_expenses
                [
                  { expense0 with amount = 10. };
                  { expense1 with amount = -10. };
                ]) );
         ( "get expenses by category with case-sensitive category" >:: fun _ ->
           assert_equal [ expense0 ]
             (get_expenses [ expense0; expense1; expense2 ] "Clothing") );
         ( "get expenses by category with case-insensitive category" >:: fun _ ->
           assert_equal []
             (get_expenses [ expense0; expense1; expense2 ] "cLOTHing") );
         ( "get categories from empty list" >:: fun _ ->
           assert_equal [] (get_categories []) );
         ( "get amount by category from empty list" >:: fun _ ->
           assert_equal [ ("Food", 0.) ] (amount_by_category [] [ "Food" ]) );
         ( "expenses above zero amount with no matching entries" >:: fun _ ->
           assert_equal [] (expenses_above [ expense0; expense1 ] 100.0) );
         ( "expenses below max float amount with no matching entries"
         >:: fun _ ->
           assert_equal [] (expenses_below [ expense0; expense1 ] 0.0) );
         ( "expenses between amount values with no matching entries" >:: fun _ ->
           assert_equal []
             (expenses_between_ammounts [ expense0; expense1 ] 100.0 200.0) );
         ( "expenses by date range with single entry before range" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0 ] "01/01/2023" "12/31/2023") );
         ( "expenses by date range with single entry after range" >:: fun _ ->
           assert_equal []
             (expenses_by_date_range [ expense0 ] "01/01/2025" "12/31/2025") );
         ( "total expenses with mixed positive and negative amount expenses"
         >:: fun _ ->
           assert_equal 45.0
             (total_expenses
                [
                  { expense0 with amount = 10. };
                  { expense1 with amount = -10. };
                  { expense2 with amount = 45. };
                ]) );
         ( "get expenses by category with multiple matching entries and mixed \
            case categories"
         >:: fun _ ->
           assert_equal []
             (get_expenses [ expense1; expense2; expense3 ] "FoOd") );
         ( "get categories from list with duplicate categories" >:: fun _ ->
           assert_equal
             [ "Entertainment"; "Food" ]
             (get_categories [ expense1; expense2; expense3 ]) );
         ( "get amount by category from list with duplicate categories"
         >:: fun _ ->
           assert_equal
             [ ("Food", 74.50); ("Entertainment", 125.99) ]
             (amount_by_category
                [ expense1; expense2; expense3 ]
                [ "Food"; "Entertainment" ]) );
         ( "expenses above zero amount with mixed positive and negative amounts"
         >:: fun _ ->
           assert_equal
             [ expense0; expense2; expense3 ]
             (expenses_above
                [
                  expense0; { expense1 with amount = -10. }; expense2; expense3;
                ]
                19.99) );
         ( "expenses below amount with mixed positive and negative amounts"
         >:: fun _ ->
           assert_equal
             [ { expense0 with amount = -10. }; expense1; expense3 ]
             (expenses_below
                [
                  { expense0 with amount = -10. }; expense1; expense2; expense3;
                ]
                100.0) );
         ( "expenses between amount values with mixed positive and negative \
            amounts"
         >:: fun _ ->
           assert_equal [ expense1; expense3 ]
             (expenses_between_ammounts
                [
                  { expense0 with amount = -10. }; expense1; expense2; expense3;
                ]
                10.0 55.0) );
         ( "expenses between amount values with mixed positive and negative \
            amounts and amount is zero"
         >:: fun _ ->
           assert_equal []
             (expenses_between_ammounts
                [
                  { expense0 with amount = -10. }; expense1; expense2; expense3;
                ]
                0.0 0.0) );
         ( "expenses between amount values with mixed positive and negative \
            amounts and amount is negative"
         >:: fun _ ->
           assert_equal [ negative_expense ]
             (expenses_between_ammounts
                [ negative_expense; expense2; expense3 ]
                (-22.0) 0.0) );
         ( "expenses by date range with multiple entries within range"
         >:: fun _ ->
           assert_equal
             [ expense0; expense1; expense2; expense3 ]
             (expenses_by_date_range
                [ expense0; expense1; expense2; expense3 ]
                "01/01/2023" "12/31/2024") );
         ( "expenses by date range with single entry on start date" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_by_date_range [ expense0 ] "04/13/2024" "12/31/2024") );
         ( "expenses by date range with multiple entry on start date"
         >:: fun _ ->
           assert_equal [ expense0; expense0 ]
             (expenses_by_date_range [ expense0; expense0 ] "04/13/2024"
                "12/31/2024") );
         ( "expenses by date range with single entry on end date" >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_by_date_range [ expense0 ] "01/01/2023" "04/13/2024") );
         ( "expenses by date range with multiple entry on end date" >:: fun _ ->
           assert_equal [ expense0; expense0 ]
             (expenses_by_date_range [ expense0; expense0 ] "01/01/2023"
                "04/13/2024") );
         ( "get expenses by category with no matching entries" >:: fun _ ->
           assert_equal []
             (get_expenses [ expense1; expense2; expense3 ] "Travel") );
         ( "get expenses by category with one matching entries" >:: fun _ ->
           assert_equal [ expense0 ]
             (get_expenses [ expense0; expense2; expense3 ] "Clothing") );
         ( "get expenses by category with  duplicate entries" >:: fun _ ->
           assert_equal [ expense2; expense2 ]
             (get_expenses [ expense2; expense2; expense3 ] "Entertainment") );
         ( "get expenses by category with multiple non-duplicate entries"
         >:: fun _ ->
           assert_equal [ expense2; expense6 ]
             (get_expenses [ expense2; expense6 ] "Entertainment") );
         ( "get categories from empty expense list" >:: fun _ ->
           assert_equal [] (get_categories []) );
         ( "amount by category from empty expense list" >:: fun _ ->
           assert_equal [] (amount_by_category [] []) );
         ( "amount by category from expense list with single entry" >:: fun _ ->
           assert_equal
             [ ("Food", 20.0) ]
             (amount_by_category [ expense1 ] [ "Food" ]) );
         ( "expenses by date range with no entries" >:: fun _ ->
           assert_equal [] (expenses_by_date_range [] "01/01/2023" "12/31/2024")
         );
         ( "expenses above zero amount with empty expense list" >:: fun _ ->
           assert_equal [] (expenses_above [] 10.0) );
         ( "expenses above negative amount with nonempty expense list"
         >:: fun _ ->
           assert_equal [ expense0 ] (expenses_above [ expense0 ] (-10.0)) );
         ( "expenses above float amount with non empty expense list" >:: fun _ ->
           assert_equal [ expense0 ] (expenses_above [ expense0 ] 0.0) );
         ( "expenses multiple above float amount with non empty expense list"
         >:: fun _ ->
           assert_equal [ expense0; expense2 ]
             (expenses_above [ expense0; expense2 ] 0.0) );
         ( "expenses multiple above float amount with non empty expense list \
            non above"
         >:: fun _ ->
           assert_equal [] (expenses_above [ expense0; expense2 ] 2000000.0) );
         ( "expenses multiple above float amount with non empty expense list \
            one above"
         >:: fun _ ->
           assert_equal [ expense2 ]
             (expenses_above [ expense0; expense2 ] 21.0) );
         ( "expenses below max float amount with empty expense list" >:: fun _ ->
           assert_equal [] (expenses_below [] 1000.0) );
         ( "expenses between amount values with empty expense list" >:: fun _ ->
           assert_equal [] (expenses_between_ammounts [] 0.0 1000.0) );
         ( "expenses between amount values with non empty expense list"
         >:: fun _ ->
           assert_equal [ expense0 ]
             (expenses_between_ammounts [ expense0 ] 19.0 21.0) );
         ( "expenses between amount values with non empty expense list"
         >:: fun _ ->
           assert_equal [ expense2 ]
             (expenses_between_ammounts [ expense0; expense2 ] 21.0
                22000000000.0) );
         ( "get expense by year with no entries" >:: fun _ ->
           assert_equal [] (get_expense_by_year [] "2024") );
         ( "get expense by year with no year" >:: fun _ ->
           assert_equal [] (get_expense_by_year [ expense0 ] "") );
         ( "get years from expenses with no entries" >:: fun _ ->
           assert_equal [] (possible_years_list []) );
         ( "percentage of total expenses by category with single category"
         >:: fun _ ->
           assert_equal
             [ ("Food", 100.0) ]
             (percentage_of_total_expenses_by_category [ expense1 ]) );
         ( "percentage of total expenses by category with multiple categories"
         >:: fun _ ->
           let expenses =
             [
               {
                 description = "Groceries";
                 category = "Food";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Clothes";
                 category = "Food";
                 amount = 30.0;
                 date = "05/15/2024";
               };
               {
                 description = "Movie Tickets";
                 category = "Entertainment";
                 amount = 20.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [ ("Entertainment", 20.); ("Food", 80.) ]
             (percentage_of_total_expenses_by_category expenses) );
         ( "percentage of total expenses by category with expenses that are \
            all different categories"
         >:: fun _ ->
           let expenses =
             [
               {
                 description = "Groceries";
                 category = "Food";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Clothes";
                 category = "Fashion";
                 amount = 30.0;
                 date = "05/15/2024";
               };
               {
                 description = "Movie Tickets";
                 category = "Entertainment";
                 amount = 20.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [ ("Entertainment", 20.); ("Fashion", 30.); ("Food", 50.) ]
             (percentage_of_total_expenses_by_category expenses) );
         ( "percentage of total expenses by category with multiple expenses \
            that are all in the same category"
         >:: fun _ ->
           let expenses =
             [
               {
                 description = "Laptop";
                 category = "Electronics";
                 amount = 1200.0;
                 date = "05/15/2024";
               };
               {
                 description = "Groceries";
                 category = "Electronics";
                 amount = 50.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [ ("Electronics", 100.) ]
             (percentage_of_total_expenses_by_category expenses) );
         ( "testing management of percentages that involve infinite decimal \
            places"
         >:: fun _ ->
           let expenses =
             [
               {
                 description = "Groceries";
                 category = "Food";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Clothes";
                 category = "Fashion";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Movie Tickets";
                 category = "Entertainment";
                 amount = 50.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [
               ("Entertainment", 33.3333333333333286);
               ("Fashion", 33.3333333333333286);
               ("Food", 33.3333333333333286);
             ]
             (percentage_of_total_expenses_by_category expenses) );
         ( "testing management of when some expenses are 0" >:: fun _ ->
           let expenses =
             [
               {
                 description = "Groceries";
                 category = "Food";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Clothes";
                 category = "Fashion";
                 amount = 0.;
                 date = "05/15/2024";
               };
               {
                 description = "Movie Tickets";
                 category = "Entertainment";
                 amount = 50.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [ ("Entertainment", 50.); ("Fashion", 0.); ("Food", 50.) ]
             (percentage_of_total_expenses_by_category expenses) );
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
         ( "testing management of when all expenses are 0 except one"
         >:: fun _ ->
           let expenses =
             [
               {
                 description = "Groceries";
                 category = "Food";
                 amount = 50.0;
                 date = "05/15/2024";
               };
               {
                 description = "Clothes";
                 category = "Fashion";
                 amount = 0.;
                 date = "05/15/2024";
               };
               {
                 description = "Movie Tickets";
                 category = "Entertainment";
                 amount = 0.0;
                 date = "05/15/2024";
               };
             ]
           in
           assert_equal
             [ ("Entertainment", 0.); ("Fashion", 0.); ("Food", 100.) ]
             (percentage_of_total_expenses_by_category expenses) );
       ]

let s = "You're too old partner! There's no point in tracking your budget now."

let budgeting_tests =
  "test suite for budgeting functions"
  >::: [
         ( "test retirement_func with future_value, discount_rate, and years 1"
         >:: fun _ ->
           assert_equal 620.921323059154929 (retirement_func 1000.0 0.1 5) );
         ( "test retirement_func with future_value, discount_rate, and years \
            no years"
         >:: fun _ -> assert_equal 1000.0 (retirement_func 1000.0 0.1 0) );
         ( "test retirement_func with future_value, discount_rate, and years \
            no money"
         >:: fun _ -> assert_equal 0. (retirement_func 0. 0. 10) );
         ( "test retirement_func with future_value, discount_rate, and years 2"
         >:: fun _ ->
           assert_equal 3069.5662677037958 (retirement_func 5000.0 0.05 10) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal s
             (required_savings_per_year 65 Safe [] 1000.0 1000.0 0.0) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You're too old partner! There's no point in tracking your budget \
              now."
             (required_savings_per_year 70 Risky [] 4000.0 4000.0 0.0) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 40 Safe expense_list1 5.0 1000.0 0.0) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 40 Safe expense_list1 16.0 800.0 0.0) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 32 Risky expense_list2 9.0 900.0 0.0) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 32 Average expense_list2 13.0 100.0 0.0)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 32 Risky expense_list2 18.0 900.0 0.0)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 1"
         >:: fun _ ->
           assert_equal
             "You are spending too much relative to your income. We suggest \
              youreview your expense breakdown. \n\
             \ Check your piechart for more information."
             (required_savings_per_year 32 Average expense_list2 10.0 900.0 0.0)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 2 full input"
         >:: fun _ ->
           assert_equal "You have to cut your budget by 50317.%"
             (required_savings_per_year 40 Average budget_list 1000. 1000000. 0.)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 3 full input"
         >:: fun _ ->
           assert_equal "You can raise your expenditure by 34316.%"
             (required_savings_per_year 50 Average budget_list 10000. 100000.
                1000.) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 3 full input"
         >:: fun _ ->
           assert_equal "You can raise your expenditure by 38105.%"
             (required_savings_per_year 50 Risky budget_list 10000. 100000.
                1000.) );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 6 full input"
         >:: fun _ ->
           assert_equal "You can raise your expenditure by 32093.%"
             (required_savings_per_year 50 Safe budget_list 10000. 100000. 1000.)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 4 full input"
         >:: fun _ ->
           assert_equal "You have to cut your budget by 28664.%"
             (required_savings_per_year 40 Risky budget_list 1000. 1000000. 0.)
         );
         ( "test required_savings_per_year with age, risk_profile, budget, \
            income, retirement_goal, and bank_balance 5 full input"
         >:: fun _ ->
           assert_equal "You have to cut your budget by 64494.%"
             (required_savings_per_year 40 Safe budget_list 1000. 1000000. 0.)
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
         ( "floats to percentages" >:: fun _ ->
           assert_equal [ 100. ]
             (get_pie_data
                (amount_by_category [ expense1 ] (get_categories [ expense1 ])))
         );
         ( "floats to percentages duplicates" >:: fun _ ->
           assert_equal [ 100. ]
             (get_pie_data
                (amount_by_category
                   [ expense0; expense0; expense0 ]
                   (get_categories [ expense0; expense0; expense0 ]))) );
         ( "floats to percentages" >:: fun _ ->
           assert_equal
             [ 20.0; 20.0; 20.0; 20.0; 20.0 ]
             (get_pie_data
                (amount_by_category
                   [ expense11; expense12; expense13; expense14; expense15 ]
                   (get_categories
                      [ expense11; expense12; expense13; expense14; expense15 ])))
         );
         ( "floats to percentages_none" >:: fun _ ->
           assert_equal []
             (get_pie_data (amount_by_category [] (get_categories []))) );
       ]

let suite =
  "overall test suite" >::: [ expenses_tests; budgeting_tests; pie_tests ]

let _ = run_test_tt_main suite
