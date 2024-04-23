open Graphics

(* Function to draw a bar *)
let draw_bar x y width height = fill_rect x y width height

(* Function to draw a bar graph *)
let draw_bar_graph year_amount_list =
  try
    let max_value =
      List.fold_left (fun acc (_, total) -> max acc total) 0.0 year_amount_list
    in
    let scale_factor = 300. /. max_value in
    let rec draw x = function
      | [] -> ()
      | (year, total) :: rest ->
          let bar_height = int_of_float (total *. scale_factor) in
          set_color red;
          draw_bar x (400 - bar_height) 50 bar_height;
          moveto (x + 25) (400 - bar_height - 10);
          draw_string year;
          draw (x + 70) rest
    in
    let width = 500 in
    let height = 500 in
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    draw 50 year_amount_list;
    (* Wait for user input before closing *)
    ignore (read_key ());
    (* Close the graphics window *)
    close_graph ()
  with Graphics.Graphic_failure _ -> close_graph ()
