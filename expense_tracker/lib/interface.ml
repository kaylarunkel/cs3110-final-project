open Bogue
module W = Widget
module L = Layout

let input_text = ref ""

let window () =
  let input = W.text_input ~max_size:200 ~prompt:"Enter your file name" () in
  let label = W.label ~size:40 "" in
  let layout =
    L.tower [ L.resident ~w:400 input; L.resident ~w:400 ~h:200 label ]
  in
  let before_display () =
    let text = W.get_text input in
    W.set_text label text;
    input_text := text
  in
  let board = Bogue.of_layout layout in
  Bogue.run ~before_display board;
  Bogue.quit ()
