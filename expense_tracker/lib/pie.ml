(* Import the Graphics module *)
open Graphics

(* Function to draw a pie chart with labels *)
let draw_pie_chart_with_labels (values : float list) (labels : string array) :
    unit =
  try
    (* Calculate total value *)
    let total = List.fold_left ( +. ) 0. values in
    (* Set up graphics window *)
    let width = 500 in
    let height = 500 in
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    (* Set up colors *)
    let colors = [| red; green; blue; yellow; cyan; magenta; black |] in
    (* Set up starting angle *)
    let start_angle = ref 0. in
    (* Draw each slice with label *)
    List.iteri
      (fun i value ->
        let slice_angle = value /. total *. 360. in
        set_color colors.(i mod Array.length colors);
        fill_arc (width / 2) (height / 2) 200 200
          (int_of_float !start_angle)
          (int_of_float (!start_angle +. slice_angle));
        let label_angle = !start_angle +. (slice_angle /. 2.) in
        let label_x =
          (width / 2)
          + int_of_float (200. *. cos (label_angle *. Float.pi /. 180.))
        in
        let label_y =
          (height / 2)
          + int_of_float (200. *. sin (label_angle *. Float.pi /. 180.))
        in
        set_color black;
        (* Change label color to black *)
        set_font "-*-times-medium-r-normal--20-*-*-*-*-*-*-*";
        moveto label_x label_y;
        draw_string labels.(i);
        start_angle := !start_angle +. slice_angle)
      values;
    (* Wait for user input before closing *)
    ignore (read_key ());
    (* Close the graphics window *)
    close_graph ()
  with Graphics.Graphic_failure _ -> close_graph ()
(* Ignore Graphic_failure exception *)

let get_pie_data (amounts : (string * float) list) =
  let total_sum =
    List.fold_left (fun acc (_, amount) -> acc +. amount) 0.0 amounts
  in
  List.map (fun (_, amount) -> amount /. total_sum *. 100.) amounts
