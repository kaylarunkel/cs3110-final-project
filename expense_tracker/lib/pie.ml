open Graphics

let colors =
  [|
    rgb 255 204 204;
    rgb 204 255 204;
    rgb 204 204 255;
    rgb 255 255 204;
    rgb 204 255 255;
    rgb 255 204 255;
    rgb 240 240 240;
    rgb 255 229 204;
    rgb 204 255 229;
    rgb 229 204 255;
  |]

let width = 600
let height = 500
let start_angle = ref 0.

let pie_chart_list_iteri total labels =
  List.iteri (fun i value ->
      let slice_angle = value /. total *. 360. in
      set_color colors.(i mod Array.length colors);
      fill_arc (width / 2) (height / 2) 200 200
        (int_of_float !start_angle)
        (int_of_float (!start_angle +. slice_angle));
      let label_angle = !start_angle +. (slice_angle /. 2.) in
      let label_x =
        (width / 2)
        + int_of_float (220. *. cos (label_angle *. Float.pi /. 180.))
      in
      let label_y =
        (height / 2)
        + int_of_float (220. *. sin (label_angle *. Float.pi /. 180.))
      in
      set_color black;
      moveto (label_x - 20) label_y;
      draw_string labels.(i);
      start_angle := !start_angle +. slice_angle)

let draw_pie_chart_with_labels (values : float list) (labels : string array) :
    unit =
  try
    let total = List.fold_left ( +. ) 0. values in
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    pie_chart_list_iteri total labels values;
    ignore (read_key ());
    start_angle := 0.;
    clear_graph ()
  with Graphics.Graphic_failure _ -> close_graph ()

let get_pie_data (amounts : (string * float) list) =
  let total_sum =
    List.fold_left (fun acc (_, amount) -> acc +. amount) 0.0 amounts
  in
  List.map (fun (_, amount) -> amount /. total_sum *. 100.) amounts
