type textbox = {
  mutable content : string;
  mutable cursor_pos : int;
}

val create_textbox : unit -> textbox
(** Creates a new textbox with empty content and cursor at position 0. *)

val draw_textbox : textbox -> int -> int -> int -> int -> unit
(** Draws the textbox on the graphics window with the specified dimensions. *)

val handle_backspace : textbox -> unit
(** Handles the backspace key press, removing the character before the cursor. *)

val handle_key_press : textbox -> char -> unit
(** Handles the key press event, adding the pressed character at the cursor
    position. *)

val read_textbox_input : unit -> string
(** Opens a graphics window and allows the user to input text into a textbox.
    Returns the entered text. *)

val open_textbox_with_prompt : string -> string
(** [open_textbox_with_prompt prompt] opens a textbox with the given prompt.
    [prompt] is the prompt to be displayed above the textbox. The function opens
    a graphical window and displays a textbox with the specified prompt. It
    allows the user to input text and handles key presses accordingly. It
    returns the input text when the Enter key is pressed. *)
