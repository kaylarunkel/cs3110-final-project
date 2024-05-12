open Expense_tracker.Expenses
open Expense_tracker.Pie
open Expense_tracker.Bar
open Expense_tracker.Button
open Expense_tracker.Textbox
open Graphics

let current = ref 0

let rec up_or_down len =
  let ev = wait_next_event [ Key_pressed; Button_down ] in
  match ev with
  | { key = 's' | 'S'; _ } ->
      current := min (!current + 1) (len - 10);
      0
  | { key = 'w' | 'W'; _ } ->
      current := max (!current - 1) 0;
      0
  | _ -> up_or_down len

let display_view_expenses_screen list =
  open_graph "";
  try
    moveto 10 400;
    draw_string "DESCRIPTION";
    moveto 210 400;
    draw_string "CATEGORY";
    moveto 410 400;
    draw_string "AMOUNT ($)";
    moveto 510 400;
    draw_string "DATE";
    let draw_entry x y expense =
      moveto x y;
      draw_string expense.description;
      moveto (x + 200) y;
      draw_string expense.category;
      moveto (x + 400) y;
      draw_string (money_string (string_of_float expense.amount));
      moveto (x + 500) y;
      draw_string expense.date
    in
    let rec draw_entries x y start acc lst =
      match lst with
      | [] -> ()
      | expense :: rest ->
          if start > 0 then draw_entries x y (start - 1) acc rest
          else if acc < 10 then (
            draw_entry x y expense;
            draw_entries x (y - 30) start (acc + 1) rest)
    in
    draw_entries 10 370 !current 0 list;
    moveto 10 50;
    draw_string "<Press [w]- up or [s]- down to see other rows>";
    let rec check_up_down () =
      if up_or_down (List.length list) = 0 then (
        clear_graph ();
        moveto 10 400;
        draw_string "DESCRIPTION";
        moveto 210 400;
        draw_string "CATEGORY";
        moveto 410 400;
        draw_string "AMOUNT";
        moveto 510 400;
        draw_string "DATE";
        draw_entries 10 370 !current 0 list;
        moveto 10 50;
        draw_string "<Press\n  [w]- up or [s]- down to see other rows>";
        check_up_down ())
    in
    check_up_down ();
    synchronize ();
    ignore (wait_next_event [ Button_down ]);
    close_graph ()
  with Graphic_failure _ -> close_graph ()

let display_total_expenses_screen list =
  open_graph "";
  try
    let total_expenses = total_expenses list in
    let total_expenses_text =
      Printf.sprintf "Total Expenses: %.2f" total_expenses
    in
    let rec find_font_size font_size =
      set_font
        (Printf.sprintf
           "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1" font_size);
      let text_width, _ = text_size total_expenses_text in
      if text_width > size_x () - 100 then find_font_size (font_size - 1)
      else font_size
    in
    let font_size = find_font_size 100 in
    set_font
      (Printf.sprintf "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1"
         font_size);
    let text_width, text_height = text_size total_expenses_text in
    let x_position = (size_x () - text_width) / 2 in
    let y_position = (size_y () - text_height) / 2 in
    moveto x_position y_position;
    draw_string total_expenses_text;
    synchronize ();
    ignore (wait_next_event [ Button_down ]);
    close_graph ()
  with Graphic_failure _ -> close_graph ()

let add_expense list =
  let description = open_textbox_with_prompt "Enter\n  description:" in
  let category = open_textbox_with_prompt "Enter category:" in
  let amount_str = open_textbox_with_prompt "Enter amount:" in
  let amount = float_of_string amount_str in
  let date = open_textbox_with_prompt "Enter date\n  (MM/DD/YYYY):" in
  let new_expense = { description; category; amount; date } in
  new_expense :: list

let rec main list =
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
  in
  let analyze_categories = [ "Pie Chart"; "Bar Graph"; "Budget" ] in
  open_graph "";
  auto_synchronize true;

  let button_spacing = 20 in
  let max_button_height = 50 in
  let button_height = min max_button_height (size_y ()) in
  let initial_x = button_spacing in
  let initial_y = (size_y () - button_height) / 2 in

  let button_width = button_size categories button_spacing in
  draw_buttons_with_positions categories initial_x initial_y button_width
    button_height button_spacing;

  let rec check_click () =
    let event = wait_next_event [ Button_down ] in
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    let clicked_button =
      find_clicked_button click_x click_y initial_x initial_y button_width
        button_height button_spacing categories
    in
    match clicked_button with
    | Some category -> handle_category category list
    | None -> check_click ()
  and handle_category category list =
    match category with
    | "View Exp." ->
        display_view_expenses_screen list;
        main list
    | "Total Exp." ->
        display_total_expenses_screen list;
        main list
    | "Read Exp." ->
        let filename = open_textbox_with_prompt "Enter CSV filename:" in
        let new_list = read_expenses_from_csv filename in
        Printf.printf "Expenses read from CSV\n  file.\n";
        main new_list
    | "Save Exp." ->
        let filename = open_textbox_with_prompt "Enter filename to save:" in
        save_expenses_to_csv filename list;
        Printf.printf "Expenses saved to CSV file.\n";
        main list
    | "Analyze" ->
        close_graph ();
        open_graph "";
        auto_synchronize true;
        draw_analyze_buttons ();
        analyze_click list
    | "Add Exp." ->
        let updated_list = add_expense list in
        main updated_list
    | "Exit" -> close_graph ()
    | _ -> check_click ()
  and analyze_click list =
    let rec handle_analyze_click () =
      let event = wait_next_event [ Button_down ] in
      let click_x = event.mouse_x in
      let click_y = event.mouse_y in
      let button_width = button_size analyze_categories button_spacing in
      let a_clicked_button =
        find_clicked_button click_x click_y initial_x initial_y button_width
          button_height button_spacing analyze_categories
      in
      match a_clicked_button with
      | Some a_category -> handle_a_category a_category list
      | None -> check_click ()
    and handle_a_category a_category list =
      match a_category with
      | "Pie Chart" ->
          open_graph "";
          auto_synchronize true;
          let textbox_for_year_pie =
            open_textbox_with_prompt
              ("Year- choose from (" ^ possible_years list ^ ")")
          in
          close_graph ();
          if
            List.mem
              (int_of_string textbox_for_year_pie)
              (possible_years_list list)
          then
            let categories = get_categories list in
            let year_expenses = get_expense_by_year list textbox_for_year_pie in
            let data =
              get_pie_data (amount_by_category year_expenses categories)
            in
            draw_pie_chart_with_labels data (Array.of_list categories)
          else (
            open_graph "";
            auto_synchronize true;
            let msg =
              "No data exists for the year you inputted (wait 3 seconds)"
            in
            let get_size_x (msg, _) = msg in
            let get_size_y (_, msg) = msg in
            moveto
              ((size_x () - get_size_x (text_size msg)) / 2)
              ((size_y () - get_size_y (text_size msg)) / 2);
            draw_string msg;
            Unix.sleep 3;
            close_graph ());
          main list
      | "Bar Graph" ->
          let yearly_amounts = total_expenses_per_year list in
          draw_bar_graph yearly_amounts;
          main list
      | "Budget" ->
          open_graph "";
          auto_synchronize true;
          let msg = "Budget info coming soon! (wait 3 seconds)" in
          let get_size_x (msg, _) = msg in
          let get_size_y (_, msg) = msg in
          moveto
            ((size_x () - get_size_x (text_size msg)) / 2)
            ((size_y () - get_size_y (text_size msg)) / 2);
          draw_string msg;
          Unix.sleep 3;
          close_graph ();
          main list
      | _ -> handle_analyze_click ()
    in
    handle_analyze_click ()
  in
  check_click ()

let draw_welcome_screen () =
  open_graph "";
  set_color black;
  moveto 200 400;
  draw_string "Budget Analyzer";

  let button_texts = [ "Load CSV"; "New CSV" ] in

  let button_spacing = 20 in
  let total_spacing = (List.length button_texts - 1) * button_spacing in
  let available_width = size_x () - total_spacing - 20 in
  let button_width = available_width / List.length button_texts in

  let initial_x =
    (size_x () - (button_width * List.length button_texts) - total_spacing) / 2
  in

  draw_buttons_with_positions button_texts initial_x 180 button_width 30
    button_spacing;

  synchronize ();

  let rec check_click () =
    let event = wait_next_event [ Button_down ] in
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    match
      find_clicked_button click_x click_y initial_x 180 button_width 30
        button_spacing button_texts
    with
    | Some "Load CSV" ->
        let filename = open_textbox_with_prompt "Enter CSV filename:" in
        let new_list = read_expenses_from_csv filename in
        Printf.printf "Expenses read from CSV\n  file.\n";
        main new_list
    | Some "New CSV" ->
        let updated_list = add_expense [] in
        main updated_list
    | _ -> check_click ()
  in
  check_click ()

let () = draw_welcome_screen ()
