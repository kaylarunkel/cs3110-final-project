type expense = {
  description : string;
  category : string;
  amount : float;
  date : string;
}

type expense_list = expense list

val add_expense :
  expense_list -> string -> string -> float -> string -> expense_list
(** [add_expense list description category amount] adds a new expense to the
    expense list.

    [list] is the current list of expenses. [description] is the description of
    the expense. [category] is the category of the expense. [amount] is the
    amount of the expense.

    Returns the updated expense list. *)

val view_expenses : expense_list -> unit
(** [view_expenses list] prints all expenses in the list.

    [list] is the list of expenses. *)

val total_expenses : expense_list -> float
(** [total_expenses list] calculates the total amount of expenses in the list.

    [list] is the list of expenses.

    Returns the total amount of expenses. *)

val read_expenses_from_csv : string -> expense_list
(** [read_expenses_from_csv filename] reads expenses from a CSV file.

    [filename] is the name of the CSV file.

    Returns the list of expenses read from the file. *)

val save_expenses_to_csv : string -> expense_list -> unit
(** [save_expenses_to_csv filename list] saves expenses to a CSV file.

    [filename] is the name of the CSV file. [list] is the list of expenses to be
    saved. *)

val get_expenses : expense_list -> string -> expense_list
(** [get_expenses list criteria] takes in the expense list and a critera (ie.
    category or date) and returns a list of expenses from that criteria *)

val get_categories : expense_list -> string list
(** [get_categories expenses] is the list of the unique categories from [list] *)

val amount_by_category : expense_list -> string list -> (string * float) list
(** [amount_by_category expenses categories] is the total expenses for each
    category *)

val expenses_by_date_range : expense_list -> string -> string -> expense list
(** [expenses_by_date_range expenses start_date end_date] is the list of
    expenses that fall into the given date range

    REQUIRES: that the format of the dates inputed into the [date] attribute of
    the records be in the form MM/DD/YYYY

    REQUIRES: that the format of the [start_date] and [end_date] be in the form
    MM/DD/YYYY *)

val expenses_above : expense_list -> float -> expense list
(** [expenses_above expenses floor] is the list of expenses that are above a
    certain specified floor value *)

val expenses_below : expense_list -> float -> expense list
(** [expenses_above expenses ceiling] is the list of expenses that are below a
    certain specified ceiling value *)

val expenses_between_ammounts : expense_list -> float -> float -> expense list
(** [expenses_between_ammounts expenses floor ceiling] is the list of expenses
    that are above a certain specified floor value and above a certain specified
    ceiling value*)

val get_year : string -> string
(** [get_year date] is the year from [date]

    REQUIRES: that the format of [date] is MM/DD/YYYY *)

val total_expenses_per_year : expense_list -> (string * float) list
(** [total_expenses_per_year expenses] is the list of the total amounts spent in
    each year *)

val sorted_by_year : (string * float) list -> (string * float) list
(** [sorted_by_year years_with_amounts] is the years and their amounts sorted by
    year in ascending order *)

val get_expense_by_year : expense_list -> string -> expense_list
(**[get_expense_by_year list year] is the list of expenses incurred in the
   particular year*)
