val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y width height text] draws the button starting at position
    (x,y) with [width] and [height] and label [text] *)

val initial_button_x : int -> string list -> int
(** [initial_button_x window_width categories] returns the left-most button's
    bottom left x position on the window. *)

val draw_buttons : int -> int -> string list -> unit
(** [draw_buttons window_width window_height categories] draws the buttons of
    [categories], appropriately positioned according to [window_width] and
    [window_height] *)

val draw_analyze_buttons : int -> int -> unit
(** [draw_analyze_buttons] draws the buttons appropriately positioned according
    to [window_width] and [window_height] *)

val find_clicked_button :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  string list ->
  string list ->
  string option
(** [find_clicked_button x y window_width window_height categories lst] is the
    name of the category of the button pressed *)

val draw_help_button : int -> int -> int -> unit
(** [draw_help_buttpm x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val find_clicked_button_with_circle :
  int -> int -> int -> int -> int -> int -> int -> string list -> string option
(** [find_clicked_button_with_circle x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val button_size : string list -> int -> int
(** [button_size categories button_spacing] is the size that each button should
    be based on the length of [categories] and the [button_spacing] *)
