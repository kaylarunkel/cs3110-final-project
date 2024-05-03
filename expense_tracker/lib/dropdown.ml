open Graphics

(* Define some constants *)

let menu_width = 100
let menu_height = 30
let menu_item_height = 20

(* Function to draw a dropdown menu *)
let draw_dropdown_menu x y items =
  (* Draw the menu button *)
  set_color blue;
  fill_rect x y menu_width menu_height;
  set_color white;
  moveto (x + 10) (y + (menu_height / 2) - 5);
  draw_string "Options";
  (* Draw the dropdown items *)
  set_color white;
  fill_rect x
    (y - (List.length items * menu_item_height))
    menu_width
    (List.length items * menu_item_height);
  set_color black;
  List.iteri
    (fun i item ->
      moveto x (y - ((i + 1) * menu_item_height));
      draw_string item)
    items

(* Function to handle mouse events *)
let rec handle_mouse_events (x : int) (y : int) items =
  let status = wait_next_event [ Button_down ] in
  let mouse_x = status.mouse_x in
  let mouse_y = status.mouse_y in
  if
    mouse_x >= x
    && mouse_x <= x + menu_width
    && mouse_y >= y
    && mouse_y <= y + menu_height
  then (
    draw_dropdown_menu x y items;
    let selected_item = wait_next_event [ Button_up ] in
    if
      selected_item.mouse_x >= x
      && selected_item.mouse_x <= x + menu_width
      && selected_item.mouse_y >= y - (List.length items * menu_item_height)
      && selected_item.mouse_y <= y
    then
      let selected_index = (y - selected_item.mouse_y) / menu_item_height in
      print_endline ("Selected item: " ^ List.nth items selected_index))
  else handle_mouse_events x y items
