val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y width height text] draws the button starting at position
    (x,y) with [width] and [height] and label [text] *)

val draw_buttons : string list -> unit
(** [draw_buttons categories] draws the buttons of [categories] *)

val draw_analyze_buttons : unit -> unit
(** [draw_analyze_buttons] draws the buttons appropriately spaced *)

val draw_buttons_with_positions :
  string list -> int -> int -> int -> int -> int -> unit
(** [draw_buttons_with_positions categories initial_x initial_y button_width button_height button_spacing]
    draws the buttons given the appropriate parameters *)

val find_clicked_button :
  int -> int -> int -> int -> int -> int -> int -> string list -> string option
(** [find_clicked_button x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val button_size : string list -> int -> int
(** [button_size categories button_spacing] is the size that each button should
    be based on the length of [categories] and the [button_spacing] *)
