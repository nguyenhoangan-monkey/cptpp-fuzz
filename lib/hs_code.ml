(* exposing the config *)
module type Bounded_int_config = sig
  val min : int
  val max : int
  val name : string
end

module Bounded_int (Config : Bounded_int_config) : sig
  type t

  val of_string : string -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  (* Here we parse the 2 char string, check digit, and manually parse digit to int *)
  (* Note: single digit printed and string are explicitly disallowed in HS code *)
  (* Note: HS code must be XX.XX.XX, any lower and is rejected *)
  let of_string s =
    let open Result.Syntax in
    
    (* internal parser. Error () is an unit to type match *)
    let parse_digits s =
      let* () = if String.length s = 2 then Ok () else Error () in
      let* c1 = match s.[0] with '0' .. '9' as c -> Ok c | _ -> Error () in
      let* c2 = match s.[1] with '0' .. '9' as c -> Ok c | _ -> Error () in
      
      let n = (int_of_char c1 - 48) * 10 + (int_of_char c2 - 48) in
      if n >= Config.min && n <= Config.max then Ok n else Error ()
    in

    (* Error accumulator *)
    match parse_digits s with
    | Ok n -> Ok n
    | Error () -> Error "Part of HS code must be exactly 2 digits"

  let to_int n = n
end

(* calling the functor *)
module Chapter = Bounded_int (struct
  let min = 1
  let max = 99
  let name = "Chapter"
end)

module Heading = Bounded_int (struct
  let min = 0
  let max = 99
  let name = "Heading"
end)

module Subheading = Bounded_int (struct
  let min = 0
  let max = 99
  let name = "Subheading"
end)


(* For the extensions, since the max length for any commodity number
  according to Data Element 7357 / TDED 7357 (WCO Data Model)
  is 22 alphanumeric characters. Subtracting from the 6 digits, we
  enforce a maximum of 16 characters for the extension.

  Plus, they are strictly enforced to be only uppercase and numbers,
  brackets and delimiters. Thus this is how we store the extension internally.
*)
module Extension : sig
  type t

  val of_string : string -> (t option, string) result
  val to_string : t -> string
end = struct
  type t = string

  let of_string s =
    match String.length s with
    | 0 -> Ok None
    | len when len > 16 -> Error "Extension exceeded 16 valid characters"
    | _ -> Ok (Some s)

  let to_string t = t
end

type t = { chapter : Chapter.t; heading : Heading.t; subheading : Subheading.t; extension : Extension.t option }

(* of_string with maximum naivety. *)
(* Smash everything into a pile of digits, cut out the first 6, *)
(* and blindly shove the original raw string into the extension. *)
let is_valid_hs_ext_char = function
  | '0' .. '9' | 'A' .. 'Z' | '[' | ']' | '(' | ')' | '*' | '.' | '-' | '/' | '_' | ':' | ' ' -> true
  | _ -> false

let of_string s =
  let open Result.Syntax in
  
  if String.length s < 6 then 
    Error "Input string too short to contain a valid prefix"
  else
    let c_raw = String.sub s 0 2 in
    let h_raw = String.sub s 2 2 in
    let s_raw = String.sub s 4 2 in
    let e_raw = 
      String.sub s 6 (String.length s - 6)
      |> String.to_seq
      |> Seq.filter is_valid_hs_ext_char 
      |> String.of_seq 
      |> String.trim 
    in

    let+ chapter = Chapter.of_string c_raw
    and+ heading = Heading.of_string h_raw
    and+ subheading = Subheading.of_string s_raw
    and+ extension = Extension.of_string e_raw in
    { chapter; heading; subheading; extension }


let of_string_exn s = match of_string s with Ok t -> t | Error msg -> failwith msg

(* Handle cases where extension = None and there is extension *)
let to_string { chapter; heading; subheading; extension } =
  let c = Chapter.to_int chapter in
  let h = Heading.to_int heading in
  let s = Subheading.to_int subheading in
  match extension with
  | None -> Printf.sprintf "%02d.%02d.%02d" c h s
  | Some e ->
      let e_str = Extension.to_string e in
      Printf.sprintf "%02d.%02d.%02d.%s" c h s e_str