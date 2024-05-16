type textbox = {
  mutable content : string;
  mutable cursor_pos : int;
}
(** Represents a textbox in which the users will type their responses*)

val open_textbox_with_prompt : string -> string
(** [open_textbox_with_prompt prompt] opens a textbox with the given prompt.
    [prompt] is the prompt to be displayed above the textbox. The function opens
    a graphical window and displays a textbox with the specified prompt. It
    allows the user to input text and handles key presses accordingly. It
    returns the input text when the Enter key is pressed. *)
