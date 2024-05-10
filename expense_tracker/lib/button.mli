val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y width height text] draws the button starting at position
    (x,y) with [width] and [height] and label [text] *)

val draw_buttons : string list -> unit
(** [draw_buttons categories] draws the buttons of [categories] *)

val draw_analyze_buttons : unit -> unit
(** [draw_analyze_buttons] draws the buttons appropriately spaced *)
