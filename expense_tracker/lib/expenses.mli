type expense = {
  description : string;  (** Description of the expense *)
  category : string;  (** Category of the expense *)
  amount : float;  (** Amount of the expense *)
  date : string;  (** Date of the expense *)
}
(** Represents an expense record *)

type expense_list = expense list
(** Represents a list of expenses *)

val add_expense :
  expense_list -> string -> string -> float -> string -> expense_list
(** [add_expense list description category amount date] adds a new expense to
    the list *)

val view_expenses : expense_list -> unit
(** [view_expenses list] prints the details of each expense in the list *)

val total_expenses : expense_list -> float
(** [total_expenses list] calculates the total amount of expenses in the list *)

val read_expenses_from_csv : string -> expense_list
(** [read_expenses_from_csv filename] reads expenses from a CSV file and returns
    them as a list *)

val save_expenses_to_csv : string -> expense_list -> unit
(** [save_expenses_to_csv filename list] saves expenses to a CSV file *)

val get_expenses : expense_list -> string -> expense_list
(** [get_expenses list criteria] retrieves expenses based on category or date
    criteria *)

val get_categories : expense_list -> string list
(** [get_categories expenses] retrieves unique categories from the list of
    expenses *)

val amount_by_category : expense_list -> string list -> (string * float) list
(** [amount_by_category expenses categories] calculates total expenses for each
    category *)

val expenses_by_date_range : expense list -> string -> string -> expense list
(** [expenses_by_date_range expenses start_date end_date] retrieves expenses
    within a date range *)

val expenses_above : expense list -> float -> expense list
(** [expenses_above expenses floor] retrieves expenses above a certain amount *)

val expenses_below : expense list -> float -> expense list
(** [expenses_below expenses ceiling] retrieves expenses below a certain amount *)

val expenses_between_ammounts : expense list -> float -> float -> expense list
(** [expenses_between_ammounts expenses floor ceiling] retrieves expenses within
    a range of amounts *)

val total_expenses_per_year : expense_list -> (string * float) list
(** [total_expenses_per_year expenses] calculates total expenses per year *)

val get_expense_by_year : expense_list -> string -> expense list
(** [get_expense_by_year list year] retrieves expenses for a specific year *)

val get_year : string -> string
(** [get_year date] returns a string representing the year for that date string *)

val sorted_by_year : (string * float) list -> (string * float) list
(** [sorted_by_year list] returns a sorted version of that list based on the
    expenses by year*)

val possible_years : expense_list -> string
(** [possible_years list] retrieves possible years in the expense list *)

val possible_years_list : expense_list -> int list
(** [possible_years_list list] retrieves possible years in the expense list as a
    list of integers *)

val money_string : string -> string
(** [money_string amount] converts a float amount to a string with proper
    formatting *)

val percentage_of_total_expenses_by_category :
  expense_list -> (string * float) list
(** [percentage_of_total_expenses_by_category expenses] calculates the
    percentage of total expenses by category *)

(** Represents different risk profiles *)
type risk_profile =
  | Safe
  | Average
  | Risky

val retirement_func : float -> float -> int -> float
(** [retirement_func future_value discount_rate years] calculates present value
    for retirement *)

val money_needed : int -> risk_profile -> float -> float -> float
(** [money_needed age risk_profile retirement_goal bank_balance] calculates
    money needed for retirement *)

val required_savings_per_year :
  int -> risk_profile -> expense_list -> float -> float -> float -> string
(** [required_savings_per_year age risk_profile budget income retirement_goal bank_balance]
    calculates required savings per year *)
