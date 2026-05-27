(* HS codes are structured as an abstract type *)

type t

val of_string : string -> (t, string) result
val of_string_exn : string -> t
val to_string : t -> string
