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
let draw_bar x y width height color total =
  set_color color;
  fill_rect x y width height;
  set_color black;
  moveto (x + (width / 2)) (y + height + 5);
  (* Positioning the value at the top *)
  draw_string (string_of_int total)

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
          let total = int_of_float total in
          (* Define total here *)
          draw_bar (x + 60) 20 40 bar_height (rgb 173 216 230) total;
          (* Light blue color *)
          moveto (x + 60 + 20) (20 + bar_height + 5);
          (* Adjusted y-coordinate *)
          (* Positioning the year label at the bottom of the bar *)
          moveto (x + 60 + 20) 10;
          (* Adjusted y-coordinate for year label *)
          draw_string year;
          draw (x + 100) rest
      (* Increased spacing between bars *)
    in
    let width = 800 in
    (* Increased window width *)
    let height = 500 in
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    (* Draw x-axis *)
    moveto 60 20;
    lineto 60 (height - 20);
    (* Extended to bottom *)
    (* Draw y-axis *)
    moveto 60 20;
    lineto (width - 20) 20;
    (* Extended to right *)
    (* Draw gridlines and y-axis labels *)
    set_color black;
    let tick_interval = max_value /. 10.0 in
    for i = 1 to 10 do
      let y_value = int_of_float (tick_interval *. float_of_int i) in
      let y_pos =
        20 + int_of_float (float_of_int ((height - 40) / 10) *. float_of_int i)
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
    let year_label = "Year" in
    let year_label_width = String.length year_label * 10 in
    (* Adjust based on font size *)
    moveto (width - 60 - year_label_width) 10;
    (* Adjusted x-coordinate *)
    draw_string year_label;
    (* Label y-axis *)
    moveto 10 (height - 20);
    draw_string "Total Expenses ($)";
    draw 0 year_amount_list;
    (* Wait for user input before closing *)
    ignore (read_key ());
    (* Close the graphics window *)
    close_graph ()
  with Graphic_failure _ -> close_graph ()
