type t

(* Constructors *)
val of_string : string -> (t, string) result
val of_string_exn : string -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit

(* Semantic Getters *)
val chapter : t -> int
val heading : t -> int
val subheading : t -> int
val extension : t -> string

(* comparison *)
val equal : t -> t -> bool
val compare : t -> t -> int
