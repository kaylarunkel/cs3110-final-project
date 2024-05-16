open Graphics

type textbox = {
  mutable content : string;
  mutable cursor_pos : int;
}

let create_textbox () = { content = ""; cursor_pos = 0 }

let draw_textbox textbox width height =
  let x = (size_x () - width) / 2 in
  let y = (size_y () - height) / 2 in
  moveto x y;
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

let draw_input_box textbox prompt =
  open_graph "";
  auto_synchronize true;
  clear_graph ();
  draw_textbox textbox (size_x () / 3) (size_y () / 8);
  moveto (size_x () / 3) (3 * size_y () / 5);
  draw_string prompt;
  synchronize ()

let rec handle_input textbox prompt =
  let event = wait_next_event [ Key_pressed ] in
  if event.keypressed then begin
    match event.key with
    | '\r' ->
        close_graph ();
        textbox.content
    | '\b' | '' ->
        handle_backspace textbox;
        draw_input_box textbox prompt;
        handle_input textbox prompt
    | _ ->
        handle_key_press textbox event.key;
        draw_input_box textbox prompt;
        handle_input textbox prompt
  end
  else handle_input textbox prompt

let open_textbox_with_prompt prompt =
  let textbox = create_textbox () in
  draw_input_box textbox prompt;
  draw_input_box textbox prompt;
  handle_input textbox prompt
