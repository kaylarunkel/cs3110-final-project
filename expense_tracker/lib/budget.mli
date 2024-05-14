type risk_profile =
  | Safe
  | Average
  | Risky

val required_savings_per_year :
  int ->
  risk_profile ->
  float ->
  float ->
  ('a * float) list ->
  ('a * float) list

val get_pie_data : (string * float) list -> float list
