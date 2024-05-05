open Graphics

let x_label_length label =
  match text_size label with
  | i, _ -> i

let draw_bar x y width height color total year =
  set_color color;
  fill_rect x y width height;
  set_color black;
  moveto
    (x + ((width - x_label_length (string_of_int total)) / 2))
    (y + height + 5);
  draw_string (string_of_int total);
  moveto (x + ((width - x_label_length year) / 2)) (y - 15);
  draw_string year

let draw_bar_graph year_amount_list =
  try
    open_graph "800x600";
    resize_window 800 600;
    let max_value =
      List.fold_left (fun acc (_, total) -> max acc total) 0.0 year_amount_list
    in
    let scale_factor = 400. /. max_value in
    let num_bars = List.length year_amount_list in
    let bar_width = 600 / num_bars in
    let rec draw x = function
      | [] -> ()
      | (year, total) :: rest ->
          let bar_height = int_of_float (total *. scale_factor) in
          let total = int_of_float total in
          draw_bar x 100 bar_width bar_height (rgb 173 216 230) total year;
          draw (x + bar_width) rest
      (* Increased spacing between bars *)
    in
    set_color black;
    moveto 100 100;
    lineto 100 500;
    moveto 100 100;
    lineto 700 100;
    for i = 1 to 10 do
      let interval = int_of_float (max_value /. 10.) in
      let y_pos = 100 + (40 * i) in
      moveto 100 y_pos;
      lineto 120 y_pos;
      moveto (100 - x_label_length (string_of_int (interval * i))) y_pos;
      draw_string (string_of_int (interval * i))
    done;
    draw 100 year_amount_list;
    (* Wait for user input before closing *)
    ignore (read_key ());
    (* Close the graphics window *)
    close_graph ()
  with Graphic_failure _ -> close_graph ()
