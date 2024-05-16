(** @author Flynn Kelleher (fpk24),
    @author Harsh Patel (hhp26),
    @author Ilia Raiszadeh (ipr6),
    @author Joonseok Jung (jj575),
    @author Kayla Runkel (kmr227) *)

open Expense_tracker.Expenses
open Expense_tracker.Pie
open Expense_tracker.Bar
open Expense_tracker.Button
open Expense_tracker.Textbox
open Graphics

type budget_profile = {
  bank : float;
  goal : float;
  age : int;
  risk : string;
  income : float;
}

let current = ref 0
let width = ref 600
let height = ref 450
let expense_list = ref []
let exit = ref 0
let redo_budget = ref 0

let help_screen_instruction =
  "<DO NOT USE THE 'x' (top left of the interface window) BUTTON TO CLOSE A \
   SCREEN>\n\
   INSTEAD, PRESS ANY KEY ON YOUR KEYBOARD TO CLOSE IT.\n\n\
  \ "

let home_help_screen_text =
  help_screen_instruction
  ^ "\n\
     Load CSV: Start with an already populated CSV of expenses.\n\
     New CSV: Add your first expense now!"

let main_help_screen_text =
  help_screen_instruction
  ^ "\n\
     View Exp: View all your expenses.\n\
     Total Exp: See the sum of your expenses.\n\
     Read CSV: Read expenses from a CSV.\n\
     Save Exp: Save all expenses to CSV.\n\
     Analyze: Analyze your expenses in a pie chart, bar graph, or budget \
     analyzer!\n\n\
     Here are the options for [Analyze]: \n\
     Pie Chart: View expenses from one year in a pie chart of categories.\n\
     Bar Graph: View your expenses in a bar graph per category.\n\
     Budget: See if you have enough for retirement!\n\n\
     Add Exp.: Add an expense to your list.\n\
     Exit: Exit the program!"

let categories =
  [
    "View Exp.";
    "Total Exp.";
    "Read Exp.";
    "Save Exp.";
    "Analyze";
    "Add Exp.";
    "Exit";
  ]

let analyze_categories = [ "Pie Chart"; "Bar Graph"; "Budget" ]
let move_to_newline y_position = y_position - 20

let draw_string_newline str x y =
  let lines = String.split_on_char '\n' str in
  let rec draw_lines lines x y =
    match lines with
    | [] -> ()
    | line :: rest ->
        moveto x y;
        draw_string line;
        let new_y_position = move_to_newline y in
        draw_lines rest x new_y_position
  in
  draw_lines lines x y

let resize_window_after_change new_width new_height =
  if new_width <> !width || new_height <> !height then (
    width := new_width;
    height := new_height;
    resize_window !width !height)

let handle_view_event len =
  if key_pressed () then
    let ev = read_key () in
    match ev with
    | 's' | 'S' ->
        current := min (!current + 1) (len - 10);
        clear_graph ();
        true
    | 'w' | 'W' ->
        current := max (!current - 1) 0;
        clear_graph ();
        true
    | _ -> false
  else false

let display_view_headers () =
  moveto (!width / 100) (14 * !height / 15);
  draw_string "DESCRIPTION";
  moveto (3 * !width / 10) (14 * !height / 15);
  draw_string "CATEGORY";
  moveto (6 * !width / 10) (14 * !height / 15);
  draw_string "AMOUNT ($)";
  moveto (8 * !width / 10) (14 * !height / 15);
  draw_string "DATE"

let draw_entry y expense =
  moveto (!width / 100) y;
  draw_string expense.description;
  moveto (3 * !width / 10) y;
  draw_string expense.category;
  moveto (6 * !width / 10) y;
  draw_string (money_string (string_of_float expense.amount));
  moveto (8 * !width / 10) y;
  draw_string expense.date

let rec draw_entries y start acc lst =
  match lst with
  | [] -> ()
  | expense :: rest ->
      if start > 0 then draw_entries y (start - 1) acc rest
      else if acc < 10 then (
        draw_entry y expense;
        draw_entries (y - (!height / 15)) start (acc + 1) rest)

let display_view_instructions () =
  moveto (!width / 100) (!height / 100);
  draw_string
    "<Press [w] - up or [s] - down to see other rows, close this window to go \
     back.>"

let display_view_check_resize list =
  resize_window_after_change (size_x ()) (size_y ());
  display_view_headers ();
  draw_entries (12 * !height / 15) !current 0 list;
  display_view_instructions ()

let rec view_expenses_loop list =
  try
    display_view_check_resize list;
    if handle_view_event (List.length list) then display_view_check_resize list;
    view_expenses_loop list
  with Graphic_failure _ -> close_graph ()

let display_view_expenses_screen list =
  try
    open_graph "";
    view_expenses_loop list
  with Graphic_failure _ -> close_graph ()

let handle_exit_screen () =
  match read_key () with
  | _ -> true

let help_screen_check_resize str =
  resize_window_after_change (size_x ()) (size_y ());
  let text =
    match str with
    | "Home" -> home_help_screen_text
    | "Main" -> main_help_screen_text
    | _ -> "No help available :("
  in
  clear_graph ();
  draw_string_newline text (!width / 100) (14 * !height / 15)

let rec help_screen_loop str =
  try
    help_screen_check_resize str;
    if handle_exit_screen () then true else help_screen_loop str
  with Graphic_failure _ -> false

let rec display_help_screen str =
  try
    open_graph "";
    if help_screen_loop str then true else display_help_screen str
  with Graphic_failure _ -> true

let move_to_x_and_y x y text =
  let text_width, text_height = text_size text in
  let x_position = (x - text_width) / 2 in
  let y_position = (y - text_height) / 2 in
  moveto x_position y_position

let move_to_x_and_y_for_titles x y text =
  let text_width, text_height = text_size text in
  let x_position = (x - text_width) / 2 in
  let y_position = y - text_height in
  moveto x_position y_position

let display_exit_instructions () =
  let instruction = "<Press any key to exit>" in
  move_to_x_and_y (size_x ()) (size_y () / 5) instruction;
  draw_string instruction

let display_total_expenses_text total_expenses_text =
  clear_graph ();
  move_to_x_and_y (size_x ()) (size_y ()) total_expenses_text;
  draw_string total_expenses_text;
  moveto (!width / 100) (!height / 100);
  display_exit_instructions ()

let display_total_check_resize total_expenses_text =
  resize_window_after_change (size_x ()) (size_y ());
  display_total_expenses_text total_expenses_text

let rec total_expenses_loop total_expenses_text =
  try
    display_total_check_resize total_expenses_text;
    if handle_exit_screen () then true
    else total_expenses_loop total_expenses_text
  with Graphic_failure _ -> false

let display_total_expenses_screen list =
  try
    open_graph "";
    let total_expenses = total_expenses list in
    let total_expenses_text =
      Printf.sprintf "Total Expenses: %.2f" total_expenses
    in
    if total_expenses_loop total_expenses_text then close_graph ()
  with Graphic_failure _ -> close_graph ()

let rec get_valid_date () =
  let date = open_textbox_with_prompt "Enter date (MM/DD/YYYY):" in
  match String.split_on_char '/' date with
  | [ month; day; year ] -> (
      try
        let _ = int_of_string month in
        let _ = int_of_string day in
        let _ = int_of_string year in
        if
          String.length month = 2
          && String.length day = 2
          && String.length year = 4
        then date
        else get_valid_date ()
      with Failure _ -> get_valid_date ())
  | _ -> get_valid_date ()

let add_expense list =
  let description = open_textbox_with_prompt "Enter description:" in
  open_graph "";
  let category = open_textbox_with_prompt "Enter category:" in
  let rec get_valid_amount () =
    let amount_str =
      open_textbox_with_prompt "Enter amount (must be a number):"
    in
    try
      let amount = float_of_string amount_str in
      if
        classify_float amount = FP_normal
        || classify_float amount = FP_subnormal
      then amount
      else get_valid_amount ()
    with Failure _ -> get_valid_amount ()
  in
  let amount = get_valid_amount () in
  let date = get_valid_date () in
  let new_expense = { description; category; amount; date } in
  new_expense :: list

let main_default () =
  resize_window_after_change (size_x ()) (size_y ());
  set_color black;
  let text = "Welcome to Your Expense Tracker" in
  move_to_x_and_y_for_titles !width (4 * !height / 5) text;
  draw_string text;
  draw_buttons !width !height categories;
  draw_help_button (9 * !width / 10) (!height - (!width / 10)) (!width / 20)

let check_click () = if button_down () then mouse_pos () else (-1, -1)

let read_expenses_refactored () =
  let filename = open_textbox_with_prompt "Enter CSV filename:" in
  expense_list := read_expenses_from_csv filename

let save_expenses_refactored () =
  let filename = open_textbox_with_prompt "Enter filename to save:" in
  save_expenses_to_csv filename !expense_list

let pie_chart_first_prompt list =
  open_textbox_with_prompt ("Year - choose from (" ^ possible_years list ^ ")")

let pie_chart_correct_year list year =
  let categories = get_categories list in
  let year_expenses = get_expense_by_year list year in
  let data = get_pie_data (amount_by_category year_expenses categories) in
  draw_pie_chart_with_labels data (Array.of_list categories);
  exit := 2

let pie_chart_incorrect_year () =
  open_graph "";
  let msg = "No data exists for the year you inputted (wait 3 seconds)" in
  let get_size_x (msg, _) = msg in
  let get_size_y (_, msg) = msg in
  moveto
    ((size_x () - get_size_x (text_size msg)) / 2)
    ((size_y () - get_size_y (text_size msg)) / 2);
  draw_string msg;
  Unix.sleep 3;
  exit := 2;
  clear_graph ()

let display_pie_chart list =
  open_graph "";
  let year = pie_chart_first_prompt list in
  close_graph ();
  if List.mem (int_of_string year) (possible_years_list list) then
    pie_chart_correct_year list year
  else pie_chart_incorrect_year ()

let float_input prompt =
  try
    let num = open_textbox_with_prompt prompt in
    float_of_string num
  with _ ->
    redo_budget := 1;
    0.0

let int_input prompt =
  try
    let num = open_textbox_with_prompt prompt in
    int_of_string num
  with _ ->
    redo_budget := 1;
    1

let display_budget_prompts () =
  open_graph "";
  let bank = float_input "Amount of money currently in your savings account:" in
  let goal = float_input "Amount of money you wish to have by retirement:" in
  let age = int_input "Enter your age:  " in
  let risk =
    open_textbox_with_prompt
      "Select risk level for budget management (Risky/Normal/Safe)"
  in
  let income = float_input "Your average annual income" in
  { bank; goal; age; risk; income }

let display_budget_result bank risk age goal income list =
  open_graph "";
  moveto 20 (size_y () / 2);
  let x = required_savings_per_year age risk list income goal bank in
  draw_string_newline x 20 (size_y () / 2)

let display_invalid_input () =
  open_graph "";
  let text = "One (or more) of your inputs are invalid (wait 3 seconds)" in
  move_to_x_and_y !width !height text;
  draw_string_newline text 20 (size_y () / 2);
  Unix.sleepf 3.0;
  exit := 2;
  clear_graph ()

let display_budget_analysis profile list =
  let bank = profile.bank in
  let risk =
    match String.lowercase_ascii profile.risk with
    | "risky" -> Risky
    | "normal" -> Average
    | _ -> Safe
  in
  let age = profile.age in
  let goal = profile.goal in
  let income = profile.income in
  if !redo_budget = 0 then display_budget_result bank risk age goal income list
  else display_invalid_input ()

let display_budget list =
  open_graph "";
  let prompt_answers = display_budget_prompts () in
  display_budget_analysis prompt_answers list;
  if !redo_budget <> 0 then redo_budget := 0
  else if handle_exit_screen () then (
    exit := 2;
    clear_graph ())

let handle_category analyse_category list =
  match analyse_category with
  | "Circular Button" -> if display_help_screen "Analyze" then ()
  | "Pie Chart" -> display_pie_chart list
  | "Bar Graph" ->
      open_graph "";
      display_exit_instructions ();
      draw_bar_graph (total_expenses_per_year list);
      exit := 2
  | "Budget" -> display_budget list
  | _ -> ()

let analyze_click list =
  let click_x, click_y = check_click () in
  let button =
    find_clicked_button click_x click_y !width !height
      (initial_button_x !width analyze_categories)
      (!height / 3) analyze_categories analyze_categories
  in
  match button with
  | Some analyse_categories -> handle_category analyse_categories list
  | _ -> ()

let rec analyse_loop list =
  resize_window_after_change (size_x ()) (size_y ());
  draw_analyze_buttons !width !height;
  analyze_click list;
  if !exit <> 2 then analyse_loop list

let analyse_refactored list : unit =
  close_graph ();
  open_graph "";
  analyse_loop list

let main_clicked_button list =
  let click_x, click_y = check_click () in
  if check_click () <> (-1, -1) then
    match
      find_clicked_button_with_circle click_x click_y !width !height
        (9 * !width / 10)
        (!height - (!width / 10))
        (!width / 20) categories
    with
    | Some "View Exp." -> display_view_expenses_screen list
    | Some "Total Exp." -> display_total_expenses_screen list
    | Some "Read Exp." -> read_expenses_refactored ()
    | Some "Save Exp." -> save_expenses_refactored ()
    | Some "Add Exp." -> expense_list := add_expense list
    | Some "Analyze" -> analyse_refactored list
    | Some "Exit" -> exit := 1
    | Some "Circular Button" ->
        if display_help_screen "Main" then clear_graph ()
    | _ -> ()
  else ()

let rec main list =
  expense_list := list;
  exit := 0;
  open_graph "";
  main_default ();
  main_clicked_button list;
  if !exit = 1 then close_graph () else main !expense_list

let welcome_screen_default categories =
  width := size_x ();
  height := size_y ();
  draw_buttons !width !height categories;
  let text = "Expense Analyser" in
  move_to_x_and_y_for_titles !width (4 * !height / 5) text;
  draw_string text;
  draw_help_button (9 * !width / 10) (!height - (!width / 10)) (!width / 20)

let load_csv_refactored () =
  let filename = open_textbox_with_prompt "Enter CSV filename:" in
  let new_list = read_expenses_from_csv filename in
  Printf.printf "Expenses read from CSV\n  file.\n";
  main new_list

let rec welcome_screen_check_resize () =
  resize_window_after_change (size_x ()) (size_y ());
  let categories = [ "Load CSV"; "New CSV" ] in
  welcome_screen_default categories;
  let click_x, click_y = check_click () in
  if check_click () <> (-1, -1) then
    match
      find_clicked_button_with_circle click_x click_y !width !height
        (9 * !width / 10)
        (!height - (!width / 10))
        (!width / 20) categories
    with
    | Some "Load CSV" -> load_csv_refactored ()
    | Some "New CSV" -> main (add_expense [])
    | Some "Circular Button" ->
        if display_help_screen "Home" then (
          clear_graph ();
          welcome_screen_check_resize ())
    | _ -> welcome_screen_check_resize ()
  else ()

let rec welcome_screen_loop () =
  try
    welcome_screen_check_resize ();
    welcome_screen_loop ()
  with Graphic_failure _ -> false

let display_welcome_screen () =
  try
    open_graph "";
    if welcome_screen_loop () then close_graph ()
  with Graphic_failure _ -> close_graph ()

let () = display_welcome_screen ()
