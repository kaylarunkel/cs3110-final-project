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

let draw_analyze_buttons () =
  let button_width = 150 in
  let button_height = 50 in
  let button_spacing = 20 in
  let total_buttons_width = (button_width * 2) + button_spacing in
  let initial_x = (Graphics.size_x () - total_buttons_width) / 2 in
  let y = (Graphics.size_y () - button_height) / 2 in
  let texts = [ "Pie Chart"; "Bar Graph" ] in
  let rec draw_buttons x = function
    | [] -> ()
    | text :: rest ->
        draw_button x y button_width button_height text;
        let next_x = x + button_width + button_spacing in
        draw_buttons next_x rest
  in
  draw_buttons initial_x texts
