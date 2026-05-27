(* hs_code.ml *)

(* For the first six digits, I want to make illegal states unrepresentable *)
(* Note: single digit printed and string are explicitly disallowed in HS code *)
(* Note: HS code must be XX.XX.XX, any lower and is rejected *)
module Chapter : sig
  type t = private int

  val make : int -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  let make n = if n >= 1 && n <= 99 then Ok n else Error "Chapter must be between 01 and 99"
  let to_int n = n
end

module Heading : sig
  type t = private int

  val make : int -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  let make n = if n >= 0 && n <= 99 then Ok n else Error "Heading must be between 00 and 99"
  let to_int n = n
end

module Subheading : sig
  type t = private int

  val make : int -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  let make n = if n >= 0 && n <= 99 then Ok n else Error "Subheading must be between 00 and 99"
  let to_int n = n
end

(* For the extensions, since the max length for any commodity number
  according to Data Element 7357 / TDED 7357 (WCO Data Model)
  is 22 alphanumeric characters. Subtracting from the 6 digits, we
  enforce a maximum of 16 characters for the extension.

  Plus, they are strictly enforced to be only uppercase and numbers,
  brackets and delimiters. Thus this is how we store the extension internally.
*)
module Extension : sig
  type t = private string

  val make : string -> (t option, string) result
end = struct
  type t = string

  let is_valid_hs_ext_char = function
    | '0' .. '9' | 'A' .. 'Z' | '[' | ']' | '(' | ')' | '*' | '.' | '-' | '/' | '_' | ':' | ' ' -> true
    | _ -> false

  let make s =
    (* one-liner to stop formatting error *)
    let cleaned = String.to_seq s |> Seq.filter is_valid_hs_ext_char |> String.of_seq |> String.trim in

    match String.length cleaned with
    | 0 -> Ok None
    | len when len > 16 -> Error "Extension exceeded 16 valid characters"
    | _ -> Ok (Some cleaned)
end

type t = { chapter : Chapter.t; heading : Heading.t; subheading : Subheading.t; extension : Extension.t option }

(* of_string with maximum naivety. *)
(* Smash everything into a pile of digits, cut out the first 6, *)
(* and blindly shove the original raw string into the extension. *)
(* AKA this is giving me an excuse to use crowbar to narrow the string universe *)
let of_string s =
  let open Result.Syntax in
  let all_digits = String.to_seq s |> Seq.filter (fun c -> '0' <= c && c <= '9') |> String.of_seq in

  if String.length all_digits < 6 then Error "Not enough digits"
  else
    (* Blindly slice the first 6 digits out of the pile *)
    (* then the extension is the rest *)
    let c_raw = String.sub all_digits 0 2 |> int_of_string in
    let h_raw = String.sub all_digits 2 2 |> int_of_string in
    let s_raw = String.sub all_digits 4 2 |> int_of_string in

    let+ chapter = Chapter.make c_raw
    and+ heading = Heading.make h_raw
    and+ subheading = Subheading.make s_raw
    and+ extension = Extension.make s in
    { chapter; heading; subheading; extension }

let of_string_exn s = match of_string s with Ok t -> t | Error msg -> failwith msg

let to_string { chapter; heading; subheading; extension } =
  let c = Chapter.to_int chapter in
  let h = Heading.to_int heading in
  let s = Subheading.to_int subheading in
  match extension with
  | None -> Printf.sprintf "%02d.%02d.%02d" c h s
  | Some e ->
      let e_str = (e :> string) in
      Printf.sprintf "%02d.%02d.%02d.%s" c h s e_str
