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

let draw_help_button x y radius =
  Graphics.set_color (Graphics.rgb 255 0 0);
  Graphics.fill_circle x y radius;
  Graphics.set_color Graphics.black;
  Graphics.set_text_size 12;
  let text_width = String.length "?" * 6 in
  let text_x = x - (text_width / 2) in
  let text_y = y - 6 in
  Graphics.moveto text_x text_y;
  Graphics.draw_string "?"

let allocated_button_width window_width categories =
  let num_categories = List.length categories in
  if num_categories > 0 then window_width / num_categories else 0

let longest_string strings =
  let rec longest_helper strings longest_so_far =
    match strings with
    | [] -> longest_so_far
    | hd :: tl ->
        let longest_rest = longest_helper tl longest_so_far in
        if String.length hd >= String.length longest_rest then hd
        else longest_rest
  in
  match strings with
  | [] -> ""
  | hd :: tl -> longest_helper tl hd

let determine_button_width allocated_width categories =
  let text_width, _ = text_size (longest_string categories) in
  let preliminary = max (allocated_width / 2) text_width in
  min preliminary allocated_width

let initial_button_x window_width categories =
  let allocated_width = allocated_button_width window_width categories in
  let button_width = determine_button_width allocated_width categories in
  (allocated_width - button_width) / 2

let draw_buttons window_width window_height categories =
  let allocated_width = allocated_button_width window_width categories in
  let button_width = determine_button_width allocated_width categories in
  let button_height = window_height / 3 in
  let initial_x = (allocated_width - button_width) / 2 in
  let initial_y = window_height / 3 in
  let rec draw_buttons_aux x y = function
    | [] -> ()
    | category :: rest ->
        let truncated_text =
          if String.length category > 13 then String.sub category 0 9 ^ "..."
          else category
        in
        draw_button x y button_width button_height truncated_text;
        let next_x = x + allocated_width in
        draw_buttons_aux next_x y rest
  in
  draw_buttons_aux initial_x initial_y categories

let rec find_clicked_button x y width height init_x init_y categories lst =
  let allocated_width = allocated_button_width width lst in
  let button_width = determine_button_width allocated_width lst in
  let button_height = height / 3 in
  match categories with
  | [] -> None
  | category :: rest ->
      if
        x >= init_x
        && x <= init_x + button_width
        && y >= init_y
        && y <= init_y + button_height
      then Some category
      else
        find_clicked_button x y width height (init_x + allocated_width) init_y
          rest lst

let is_within_circular_button x y cx cy radius =
  let dx = x - cx in
  let dy = y - cy in
  (dx * dx) + (dy * dy) <= radius * radius

let find_clicked_button_with_circle x y width height circle_x circle_y
    circle_radius categories =
  let allocated_width = allocated_button_width width categories in
  let button_width = determine_button_width allocated_width categories in
  let init_x = (allocated_width - button_width) / 2 in
  let init_y = height / 3 in
  if is_within_circular_button x y circle_x circle_y circle_radius then
    Some "Circular Button"
  else
    match
      find_clicked_button x y width height init_x init_y categories categories
    with
    | Some i -> Some i
    | _ -> None

(*let find_clicked_button_rows (x : int) (y : int) (initial_x : int) (initial_y
  : int) (button_width : int) (button_height : int) (button_spacing : int)
  (num_rows : int) (categories : string list) = let rec find_clicked_button_aux
  row remaining_categories = match remaining_categories with | [] -> None |
  categories_row -> let y_row = initial_y - ((row - 1) * (button_height +
  button_spacing)) in if y >= y_row && y <= y_row + button_height then let
  clicked_category = find_clicked_button x y

  x y_row initial_x initial_y button_width button_height button_spacing
  categories_row in match clicked_category with | Some category -> Some category
  | None -> if row < num_rows then find_clicked_button_aux (row + 1)
  remaining_categories else None else find_clicked_button_aux (row + 1)
  remaining_categories in find_clicked_button_aux 1 categories*)

let button_size (categories : string list) (button_spacing : int) : int =
  let available_space =
    size_x () - ((List.length categories + 1) * button_spacing)
  in
  let max_button_width = available_space / List.length categories in
  max_button_width

(*let button_size_num num_categories button_spacing = let available_space =
  size_x () - ((num_categories + 1) * button_spacing) in let max_button_width =
  available_space / num_categories in max_button_width*)

let draw_analyze_buttons window_width window_height =
  let texts = [ "Pie Chart"; "Bar Graph"; "Budget" ] in
  draw_buttons window_width window_height texts

(*let draw_buttons_in_rows (num_rows : int) (categories : string list)
  (initial_x : int) (initial_y : int) (button_height : int) (button_spacing :
  int) = let total_buttons = List.length categories in let max_buttons_per_row =
  total_buttons / num_rows in let button_width = button_size_num (List.length
  categories / num_rows) button_spacing in

  let rec take_n_elements n lst acc = match (lst, n) with | _, 0 -> (List.rev
  acc, lst) | [], _ -> (List.rev acc, []) | x :: xs, _ -> take_n_elements (n -
  1) xs (x :: acc) in

  let rec draw_buttons_aux x y buttons_remaining = match buttons_remaining with
  | [] -> () | buttons -> let current_row_buttons, remaining_buttons =
  take_n_elements max_buttons_per_row buttons [] in let row_start_x =
  button_spacing in let rec draw_row_buttons row_x = function | [] -> () |
  category :: rest -> let truncated_text = if String.length category > 13 then
  String.sub category 0 9 ^ "..." else category in draw_button row_x y
  button_width button_height truncated_text; let next_x = row_x + button_width +
  button_spacing in draw_row_buttons next_x rest in draw_row_buttons row_start_x
  current_row_buttons; draw_buttons_aux x (y - (button_height + button_spacing))
  remaining_buttons in draw_buttons_aux initial_x initial_y categories*)

(*let dropdown_menu sizey categories = let button_height = 50 in let
  button_spacing = 18 in

  let num_rows = 2 in

  let initial_x = button_spacing in let initial_y = sizey / 2 in

  let button_width = button_size_num (List.length categories / 2) button_spacing
  in

  draw_buttons_in_rows num_rows categories initial_x initial_y button_height
  button_spacing;

  let rec handle_events () = let status = Graphics.wait_next_event [
  Graphics.Button_down ] in let x = status.Graphics.mouse_x in let y =
  status.Graphics.mouse_y in let clicked_category = find_clicked_button x y
  find_clicked_button_rows x y initial_x initial_y button_width button_height
  button_spacing 2 categories in match clicked_category with | Some category ->
  category | None -> handle_events () in handle_events ()*)
