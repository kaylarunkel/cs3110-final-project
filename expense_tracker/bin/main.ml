open Expense_tracker.Expenses
open Expense_tracker.Pie
open Expense_tracker.Bar
open Expense_tracker.Button
open Expense_tracker.Textbox
open Graphics

let draw_buttons_with_positions categories initial_x initial_y button_width
    button_height button_spacing =
  let rec draw_buttons_aux x y = function
    | [] -> ()
    | category :: rest ->
        let truncated_text =
          if String.length category > 13 then String.sub category 0 9 ^ "..."
          else category
        in
        draw_button x y button_width button_height truncated_text;
        let next_x = x + button_width + button_spacing in
        draw_buttons_aux next_x y rest
  in
  draw_buttons_aux initial_x initial_y categories

let rec find_clicked_button x y initial_x initial_y button_width button_height
    button_spacing categories =
  match categories with
  | [] -> None
  | category :: rest ->
      if
        x >= initial_x
        && x <= initial_x + button_width
        && y >= initial_y
        && y <= initial_y + button_height
      then Some category
      else
        let next_x = initial_x + button_width + button_spacing in
        find_clicked_button x y next_x initial_y button_width button_height
          button_spacing rest

let display_total_expenses_screen list =
  open_graph " 800x600";
  let total_expenses = total_expenses list in
  let total_expenses_text =
    Printf.sprintf "Total Expenses: %.2f" total_expenses
  in
  let rec find_font_size font_size =
    set_font
      (Printf.sprintf "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1"
         font_size);
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

let open_textbox_with_prompt prompt =
  open_graph " 800x600";
  let textbox = create_textbox () in
  let draw_input_box () =
    clear_graph ();
    draw_textbox textbox 100 400 200 30;
    set_color black;
    moveto 100 430;
    draw_string prompt;
    synchronize ()
  in
  let rec handle_input () =
    let event = wait_next_event [ Key_pressed ] in
    if event.keypressed then begin
      match event.key with
      | '\r' ->
          close_graph ();
          textbox.content
      | '\b' | '' ->
          if textbox.cursor_pos > 0 then begin
            textbox.content <-
              String.sub textbox.content 0 (textbox.cursor_pos - 1)
              ^ String.sub textbox.content textbox.cursor_pos
                  (String.length textbox.content - textbox.cursor_pos);
            textbox.cursor_pos <- textbox.cursor_pos - 1;
            draw_input_box ();
            handle_input ()
          end
          else handle_input ()
      | _ ->
          handle_key_press textbox event.key;
          draw_input_box ();
          handle_input ()
    end
    else handle_input ()
  in
  draw_input_box ();
  let input = handle_input () in
  input

let add_expense list =
  let description = open_textbox_with_prompt "Enter description:" in
  let category = open_textbox_with_prompt "Enter category:" in
  let amount_str = open_textbox_with_prompt "Enter amount:" in
  let amount = float_of_string amount_str in
  let date = open_textbox_with_prompt "Enter date (MM/DD/YYYY):" in
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
  open_graph "";

  let button_spacing = 20 in
  let max_text_length =
    List.fold_left
      (fun acc category -> max acc (String.length category))
      0 categories
  in
  let max_button_height = 50 in
  let button_height = min max_button_height (size_y ()) in
  let available_space =
    size_x () - ((List.length categories + 1) * button_spacing)
  in
  let max_button_width = available_space / List.length categories in
  let initial_x = button_spacing in
  let initial_y = (size_y () - button_height) / 2 in

  let button_width =
    min max_button_width (max_text_length * 10)
    (* Adjusted button width *)
  in

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
        open_graph "";
        view_expenses list;
        main list
    | "Total Exp." ->
        display_total_expenses_screen list;
        main list
    | "Read Exp." ->
        let filename = open_textbox_with_prompt "Enter CSV filename:" in
        let new_list = read_expenses_from_csv filename in
        Printf.printf "Expenses read from CSV file.\n";
        main new_list
    | "Save Exp." ->
        let filename = open_textbox_with_prompt "Enter filename to save:" in
        save_expenses_to_csv filename list;
        Printf.printf "Expenses saved to CSV file.\n";
        main list
    | "Analyze" ->
        close_graph ();
        open_graph "";
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
      let initial_x = (size_x () - 320) / 2 in
      let initial_y = (size_y () - 50) / 2 in
      if
        click_x >= initial_x
        && click_x <= initial_x + 150
        && click_y >= initial_y
        && click_y <= initial_y + 50
      then begin
        open_graph "";
        let textbox_for_year_pie = open_textbox_with_prompt "Year (YYYY)" in
        close_graph ();
        let categories = get_categories list in
        let year_expenses = get_expense_by_year list textbox_for_year_pie in
        let data = get_pie_data (amount_by_category year_expenses categories) in
        draw_pie_chart_with_labels data (Array.of_list categories);
        main list
      end
      else if
        click_x >= initial_x + 150 + 20
        && click_x <= initial_x + 150 + 20 + 150
        && click_y >= initial_y
        && click_y <= initial_y + 50
      then begin
        let yearly_amounts = total_expenses_per_year list in
        draw_bar_graph yearly_amounts;
        main list
      end
      else handle_analyze_click ()
    in
    handle_analyze_click ()
  in

  check_click ()

let () = main []
