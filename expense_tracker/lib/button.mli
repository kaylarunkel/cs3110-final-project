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

val draw_help_button : int -> int -> int -> unit
(** [draw_help_buttpm x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val find_clicked_button_with_circle :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  string list ->
  int ->
  int ->
  int ->
  string option
(** [find_clicked_button_with_circle x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val find_clicked_button_rows :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  string list ->
  string option
(** [find_clicked_button x y initial_x initial_y button_width button_height button_spacing categories]
    is the name of the category of the button pressed *)

val button_size : string list -> int -> int
(** [button_size categories button_spacing] is the size that each button should
    be based on the length of [categories] and the [button_spacing] *)

val dropdown_menu : int -> string list -> string
(** [dropdown_menu sizey categories] is a function that creates a dropdown menu
    with buttons for categories. [sizey] is the vertical size of the menu.
    [categories] is a list of strings representing the categories to be
    displayed. The function calculates the positions and sizes of buttons based
    on the number of categories and draws them on the screen. It then waits for
    user input to detect which category button is clicked. *)

val draw_buttons_in_rows :
  int -> string list -> int -> int -> int -> int -> unit
(** [draw_buttons_in_rows num_rows categories initial_x initial_y button_height button_spacing]
    draws buttons for categories in rows. [num_rows] is the number of rows to
    display the buttons in. [categories] is a list of strings representing the
    categories. [initial_x] is the initial x-coordinate for drawing buttons.
    [initial_y] is the initial y-coordinate for drawing buttons. [button_height]
    is the height of each button. [button_spacing] is the spacing between
    buttons. The function calculates the positions and sizes of buttons based on
    the number of categories and draws them in rows on the screen. *)
