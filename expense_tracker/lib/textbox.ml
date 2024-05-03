open Graphics

type textbox = {
  mutable content : string;
  mutable cursor_pos : int;
}

let create_textbox () = { content = ""; cursor_pos = 0 }

let draw_textbox textbox x y width height =
  set_color black;
  fill_rect x y width height;
  set_color white;
  fill_rect (x + 1) (y + 1) (width - 2) (height - 2);
  set_color black;
  moveto (x + 5) (y + (height / 2));
  draw_string textbox.content;
  if textbox.cursor_pos = String.length textbox.content then draw_string "_"
  else draw_string (String.sub textbox.content textbox.cursor_pos 1)

let handle_backspace textbox =
  if textbox.cursor_pos > 0 then begin
    textbox.content <-
      String.sub textbox.content 0 (textbox.cursor_pos - 1)
      ^ String.sub textbox.content textbox.cursor_pos
          (String.length textbox.content - textbox.cursor_pos);
    textbox.cursor_pos <- textbox.cursor_pos - 1
  end

let handle_key_press textbox key =
  textbox.content <-
    String.sub textbox.content 0 textbox.cursor_pos
    ^ Char.escaped key
    ^ String.sub textbox.content textbox.cursor_pos
        (String.length textbox.content - textbox.cursor_pos);
  textbox.cursor_pos <- textbox.cursor_pos + 1

let read_textbox_input () =
  open_graph " 800x600";
  let textbox = create_textbox () in
  let draw_input_box () =
    clear_graph ();
    draw_textbox textbox 100 400 200 30;
    synchronize ()
  in
  let rec handle_input () =
    let event = wait_next_event [ Key_pressed ] in
    if event.keypressed then begin
      match event.key with
      | '\r' ->
          (* Enter key *)
          close_graph ();
          textbox.content
      | '\b' | '' ->
          (* Backspace or Delete *)
          handle_backspace textbox;
          draw_input_box ();
          handle_input ()
      | _ ->
          handle_key_press textbox event.key;
          draw_input_box ();
          handle_input ()
    end
    else handle_input ()
  in
  draw_input_box ();
  let input = handle_input () in
  input
