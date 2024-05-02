(* open Graphics

   (* Function to draw a bar *) let draw_bar x y width height = fill_rect x y
   width height

   (* Function to draw a bar graph *) let draw_bar_graph year_amount_list = try
   let max_value = List.fold_left (fun acc (_, total) -> max acc total) 0.0
   year_amount_list in let scale_factor = 300. /. max_value in let rec draw x =
   function | [] -> () | (year, total) :: rest -> let bar_height = int_of_float
   (total *. scale_factor) in set_color red; draw_bar x (400 - bar_height) 50
   bar_height; moveto (x + 25) (400 - bar_height - 10); draw_string year; draw
   (x + 70) rest in let width = 500 in let height = 500 in open_graph (" " ^
   string_of_int width ^ "x" ^ string_of_int height); draw 50 year_amount_list;
   (* Wait for user input before closing *) ignore (read_key ()); (* Close the
   graphics window *) close_graph () with Graphics.Graphic_failure _ ->
   close_graph () *)
open Graphics

(* Function to draw a bar *)
let draw_bar x y width height color =
  set_color color;
  fill_rect x y width height

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
          let color = if total > max_value *. 0.7 then blue else green in
          let width = 40 in
          (* Set width of bars *)
          draw_bar (x + 60) 20 width bar_height color;
          moveto (x + 80) 10;
          draw_string year;
          moveto 30 (20 + bar_height);
          draw_string (string_of_float total);
          draw (x + 60) rest
    in
    let width = 600 in
    let height = 500 in
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    (* Draw x-axis *)
    moveto 60 20;
    lineto 60 420;
    (* Draw y-axis *)
    moveto 60 20;
    lineto 560 20;
    (* Draw gridlines and y-axis labels *)
    set_color black;
    let tick_interval = max_value /. 10.0 in
    for i = 1 to 10 do
      let y_value = int_of_float (tick_interval *. float_of_int i) in
      let y_pos =
        20 + int_of_float (float_of_int (400 / 10) *. float_of_int i)
      in
      moveto 60 y_pos;
      lineto 70 y_pos;
      moveto 30 y_pos;
      draw_string (string_of_int y_value)
    done;
    (* Draw title *)
    moveto ((width / 2) - 100) (height - 20);
    set_text_size 20;
    draw_string "Expense Tracker";
    set_text_size 12;
    (* Label x-axis *)
    moveto (width - 60) 10;
    draw_string "Year";
    (* Label y-axis *)
    moveto 10 (height - 20);
    draw_string "Total Expenses ($)";
    draw 0 year_amount_list;
    (* Wait for user input before closing *)
    ignore (read_key ());
    (* Close the graphics window *)
    close_graph ()
  with Graphic_failure _ -> close_graph ()
