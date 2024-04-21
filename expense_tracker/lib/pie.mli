val draw_pie_chart_with_labels : float list -> string array -> unit
(** [draw_pie_chart_with_labels values labels] is the pie chart of the [values]
    for each [labels] *)

val get_pie_data : (string * float) list -> float list
(** [get_pie_data amounts] is [amounts] as percentages *)
