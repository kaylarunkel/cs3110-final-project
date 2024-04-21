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
