signature GETOPT =
sig

  (* What to do with options following non-options.
  *)
  datatype 'a arg_order =
    REQUIRE_ORDER (* No option processing after first non-option. *)
  | PERMUTE (* Freely intersperse options and non-options. *)
  | RETURN_IN_ORDER of string -> 'a (* Wrap non-options into options *)

  (* Describes whether an option takes an argument or not, and if so how
  the argument is injected into a value of type a. *)
  datatype 'a arg_descr =
    NO_ARG of (unit -> 'a)
  | REQ_ARG of (string -> 'a) * string
  | OPT_ARG of (string option -> 'a) * string

  (* Each opt_descr describes a single option.

     The fields are:

       - short: list of short option characters

       - long: list of long option strings (without "--")

       - arg: argument descriptor

       - desc: explanation of option for user
  *)
  type 'a opt_descr =
    {short: char list, long: string list, arg: 'a arg_descr, desc: string}

  (* Produce a summary of the available options. *)
  val usage: 'a opt_descr list -> string

(* Process the command-line, and return the list of values that
matched (and those that didn't). The arguments are:

    - The order requirements (see arg_order)

    - The option descriptions (see opt_descr)

    - The actual command line arguments (presumably got from CommandLine.arguments).

getOpt returns a triple consisting of the option arguments, a list of
non-options, and a list of error messages.
*)
  val getopt: 'a arg_order
              -> 'a opt_descr list
              -> string list
              -> 'a list * string list * string list

end
