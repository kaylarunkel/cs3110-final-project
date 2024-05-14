open Graphics

let draw_button x y width height text =
  Graphics.set_color (Graphics.rgb 220 220 220);
  Graphics.fill_rect x y width height;
  Graphics.set_color Graphics.black;
  Graphics.set_text_size 12;
  let text_x = x + (width / 2) - (String.length text * 6 / 2) in
  let text_y = y + (height / 2) - 6 in
  Graphics.moveto text_x text_y;
  Graphics.draw_string text

let draw_buttons categories =
  let num_categories = List.length categories in
  let button_spacing = 20 in
  let total_spacing = button_spacing * (num_categories - 1) in
  let max_text_length =
    List.fold_left
      (fun acc category -> max acc (String.length category))
      0 categories
  in
  let max_button_width = min (max_text_length * 10) 70 + 20 in
  let max_button_height = 50 in
  let button_height = min max_button_height (size_y ()) in
  let max_total_width = (max_button_width * num_categories) + total_spacing in
  let max_button_width =
    if max_total_width > size_x () then
      (size_x () - total_spacing - (button_spacing / 2)) / num_categories
    else max_button_width
  in
  let initial_x =
    (size_x ()
    - (max_button_width * num_categories)
    - total_spacing + button_spacing)
    / 2
  in
  let initial_y = (size_y () - button_height) / 2 in
  let rec draw_buttons_aux x y = function
    | [] -> ()
    | category :: rest ->
        let truncated_text =
          if String.length category > 13 then String.sub category 0 9 ^ "..."
          else category
        in
        draw_button x y max_button_width button_height truncated_text;
        let next_x = x + max_button_width + button_spacing in
        draw_buttons_aux next_x y rest
  in
  draw_buttons_aux initial_x initial_y categories

(*let draw_analyze_buttons () = let button_width = 150 in let button_height = 50
  in let button_spacing = 20 in let total_buttons_width = (button_width * 3) +
  button_spacing in let initial_x = (Graphics.size_x () - total_buttons_width) /
  3 in let y = (Graphics.size_y () - button_height) / 2 in let texts = [ "Pie
  Chart"; "Bar Graph"; "Budget" ] in let rec draw_buttons x = function | [] ->
  () | text :: rest -> draw_button x y button_width button_height text; let
  next_x = x + button_width + button_spacing in draw_buttons next_x rest in
  draw_buttons initial_x texts*)

let draw_buttons_with_positions (categories : string list) (initial_x : int)
    (initial_y : int) (button_width : int) (button_height : int)
    (button_spacing : int) =
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

let rec find_clicked_button (x : int) (y : int) (initial_x : int)
    (initial_y : int) (button_width : int) (button_height : int)
    (button_spacing : int) (categories : string list) =
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
        let next_initial_x, next_initial_y =
          if next_x + button_width > size_x () then
            (button_spacing, initial_y - (button_height + button_spacing))
          else if next_x + button_width <= size_x () && next_x > size_x () then
            (button_spacing, initial_y - (2 * (button_height + button_spacing)))
          else (next_x, initial_y)
        in
        find_clicked_button x y next_initial_x next_initial_y button_width
          button_height button_spacing rest

let find_clicked_button_rows (x : int) (y : int) (initial_x : int)
    (initial_y : int) (button_width : int) (button_height : int)
    (button_spacing : int) (num_rows : int) (categories : string list) =
  let rec find_clicked_button_aux row remaining_categories =
    match remaining_categories with
    | [] -> None
    | categories_row ->
        let y_row =
          initial_y - ((row - 1) * (button_height + button_spacing))
        in
        if y >= y_row && y <= y_row + button_height then
          let clicked_category =
            find_clicked_button x y_row initial_x initial_y button_width
              button_height button_spacing categories_row
          in
          match clicked_category with
          | Some category -> Some category
          | None ->
              if row < num_rows then
                find_clicked_button_aux (row + 1) remaining_categories
              else None
        else find_clicked_button_aux (row + 1) remaining_categories
  in
  find_clicked_button_aux 1 categories

let button_size (categories : string list) (button_spacing : int) : int =
  let available_space =
    size_x () - ((List.length categories + 1) * button_spacing)
  in
  let max_button_width = available_space / List.length categories in
  max_button_width

let button_size_num num_categories button_spacing =
  let available_space = size_x () - ((num_categories + 1) * button_spacing) in
  let max_button_width = available_space / num_categories in
  max_button_width

let draw_analyze_buttons () =
  let button_height = 50 in
  let button_spacing = 20 in
  let texts = [ "Pie Chart"; "Bar Graph"; "Budget" ] in
  let initial_y = (size_y () - button_height) / 2 in
  let button_width = button_size texts button_spacing in
  let initial_x = button_spacing in
  draw_buttons_with_positions texts initial_x initial_y button_width
    button_height button_spacing

let draw_buttons_in_rows (num_rows : int) (categories : string list)
    (initial_x : int) (initial_y : int) (button_height : int)
    (button_spacing : int) =
  let total_buttons = List.length categories in
  let max_buttons_per_row = total_buttons / num_rows in
  let button_width =
    button_size_num (List.length categories / num_rows) button_spacing
  in

  let rec take_n_elements n lst acc =
    match (lst, n) with
    | _, 0 -> (List.rev acc, lst)
    | [], _ -> (List.rev acc, [])
    | x :: xs, _ -> take_n_elements (n - 1) xs (x :: acc)
  in

  let rec draw_buttons_aux x y buttons_remaining =
    match buttons_remaining with
    | [] -> ()
    | buttons ->
        let current_row_buttons, remaining_buttons =
          take_n_elements max_buttons_per_row buttons []
        in
        let row_start_x = button_spacing in
        let rec draw_row_buttons row_x = function
          | [] -> ()
          | category :: rest ->
              let truncated_text =
                if String.length category > 13 then
                  String.sub category 0 9 ^ "..."
                else category
              in
              draw_button row_x y button_width button_height truncated_text;
              let next_x = row_x + button_width + button_spacing in
              draw_row_buttons next_x rest
        in
        draw_row_buttons row_start_x current_row_buttons;
        draw_buttons_aux x
          (y - (button_height + button_spacing))
          remaining_buttons
  in
  draw_buttons_aux initial_x initial_y categories

let dropdown_menu sizey categories =
  let button_height = 50 in
  let button_spacing = 18 in

  let num_rows = 2 in

  let initial_x = button_spacing in
  let initial_y = sizey / 2 in

  let button_width =
    button_size_num (List.length categories / 2) button_spacing
  in

  draw_buttons_in_rows num_rows categories initial_x initial_y button_height
    button_spacing;

  let rec handle_events () =
    let status = Graphics.wait_next_event [ Graphics.Button_down ] in
    let x = status.Graphics.mouse_x in
    let y = status.Graphics.mouse_y in
    let clicked_category =
      find_clicked_button_rows x y initial_x initial_y button_width
        button_height button_spacing 2 categories
    in
    match clicked_category with
    | Some category -> category
    | None -> handle_events ()
  in
  handle_events ()
