(* exposing the config *)
module type Bounded_int_config = sig
  val min : int
  val max : int
  val name : string
end

(* Here we parse the 2 char string, check digit, and manually parse digit to int *)
(* Note: single digit printed and string are explicitly disallowed in HS code *)
(* Note: HS code must be XX.XX.XX, any lower and is rejected *)
module Bounded_int (Config : Bounded_int_config) : sig
  type t

  val of_string : string -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  let of_string s =
    let open Result.Syntax in

    let* () =
      if String.length s = 2 then Ok ()
      else Error (Printf.sprintf "%s segment must be exactly 2 characters long (received %S)" Config.name s)
    in
    let* c1 = match s.[0] with
      | '0' .. '9' as c -> Ok c
      | _ -> Error (Printf.sprintf "Invalid character %C in %s segment; must be a digit" s.[0] Config.name)
    in
    let* c2 = match s.[1] with
      | '0' .. '9' as c -> Ok c
      | _ -> Error (Printf.sprintf "Invalid character %C in %s segment; must be a digit" s.[1] Config.name)
    in

    let n = (int_of_char c1 - 48) * 10 + (int_of_char c2 - 48) in
    if n >= Config.min && n <= Config.max then
      Ok n
    else
      Error (Printf.sprintf "%s value %02d is out of bounds (must be between %02d and %02d)"
               Config.name n Config.min Config.max)

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

  Here, we only check the characters in place. There is no state machine.
*)
module Extension : sig
  type t
  val of_string : string -> (t, string) result
  val to_string : t -> string
end = struct
  type t = string

  let of_string s =
    if String.length s = 0 then Error "Extension cannot be empty"
    else if String.length s > 16 then Error "Extension exceeded 16 characters"
    else Ok s

  let to_string t = t
end

type t = { chapter : Chapter.t; heading : Heading.t; subheading : Subheading.t; extension : Extension.t option }

type prefix_result =
  | ValidPrefix of string * string
  | Invalid of string

let prefix_parser raw_s len =
  let rec verify i last =
    if i >= len then Ok ()
    else
      let c = raw_s.[i] in
      match last, c with
      | '-', '_' -> Error (Printf.sprintf "Illegal sequence '-_' at position %d" (i + 1))
      | '_', '-' -> Error (Printf.sprintf "Illegal sequence '_-' at position %d" (i + 1))
      | ('.'|'/'), ('.'|'/') -> Error (Printf.sprintf "Illegal consecutive delimiters at position %d" (i + 1))
      | _ -> verify (i + 1) c
  in

  let rec collect i digits_acc =
    let digits_count = List.length digits_acc in
    if digits_count = 6 then
      Some (List.rev digits_acc, i)
    else if i >= len then
      None
    else
      match raw_s.[i] with
      | '0'..'9' as digit -> 
          collect (i + 1) (digit :: digits_acc)
      | ' ' | '_' | '-' -> 
          collect (i + 1) digits_acc
      | '.' | '/' ->
          (* Delimiters can only appear after Chapter or Heading *)
          begin match digits_count with
          | 2 | 4 -> collect (i + 1) digits_acc
          | _ -> None
          end    
      | _ -> None
  in

  (* initiate matching logic *)
  match verify 0 '\000' with
  | Error msg -> Error msg
  | Ok () ->
      match collect 0 [] with
      | None -> Error "Malformed HS code prefix structure or illegal characters detected"
      | Some (digits, ext_start_idx) -> Ok (digits, ext_start_idx)

let prefix_unicode_parser raw_s =
  let len = String.length raw_s in
  match prefix_parser raw_s len with
  | Error msg -> Invalid msg
  | Ok (digits, ext_start_idx) ->
      let prefix = String.of_seq (List.to_seq digits) in
      let extension = String.sub raw_s ext_start_idx (len - ext_start_idx) in
      ValidPrefix (prefix, extension)


(* tracking the state of brackets *)
type bracket_context =
| Outside
| Inside of char * bool

(* Now, we clean and check whether it has any semantic meaning *)
(* The caller has the responsibility to enforce data hierarchy. *)
(* because of it, we use a state machine. *)
let extension_validator raw_s uchars =
  let rec loop ctx streak chars =
    match chars with
    | [] ->
        (match ctx with
         | Outside -> ValidPrefix (raw_s, raw_s)
         | Inside _ -> Invalid "Unclosed bracket at end of extension")
    | u :: rest ->
        let code = Uchar.to_int u in
        if code < 0 || code > 127 then
          Invalid "Multi-byte/Non-ASCII characters are not allowed"
        else
          let c = Char.chr code in
          match c with
          | ' ' ->
              loop ctx streak rest

          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' ->
              let next_ctx = match ctx with
                | Outside -> Outside
                | Inside (close, _) -> Inside (close, true)
              in
              loop next_ctx None rest

          | '[' | '(' ->
              (match ctx with
              | Inside _ -> Invalid "Nested brackets are not allowed"
              | Outside ->
                  let expected_close = if c = '[' then ']' else ')' in
                  loop (Inside (expected_close, false)) None rest)

          | ']' | ')' ->
              (match ctx with
              | Outside -> Invalid "Unmatched closing bracket"
              | Inside (expected, has_alnum) ->
                  if c <> expected then Invalid "Mismatched closing bracket type"
                  else if not has_alnum then Invalid "Bracket contains no alphanumeric characters"
                  else loop Outside None rest)

          | '-' | '/' | ':' | '_' | '.' ->
              (match streak with
              | None -> loop ctx (Some c) rest
              | Some last_delim ->
                  if c <> last_delim then Invalid "Heterogeneous continuous delimiters detected"
                  else loop ctx (Some c) rest)

          | _ ->
              Invalid (Printf.sprintf "Invalid character '%c' in extension" c)
  in
  loop Outside None uchars

let extension_unicode_validator raw_s =
  let decoder = Uutf.decoder (`String raw_s) in
  let rec decode_all acc =
    match Uutf.decode decoder with
    | `Await -> Invalid "Unexpected streaming block"
    | `Malformed _ -> Invalid "Input contains invalid UTF-8 byte sequences"
    | `End -> extension_validator raw_s (List.rev acc)
    | `Uchar uchar -> decode_all (uchar :: acc)
  in
  decode_all []


(* here we flatten the implicit hierarchy of the extension for the densest *)
(* most compact representation, as recommended by the *)
(* Data Element 7357 / TDED 7357 (WCO Data Model). *)
let extension_parser uchars =
  (* acc holds the characters in reverse order, count tracks length dynamically *)
  let rec loop acc count chars =
    match chars with
    | [] ->
        (match count with
        | 0 -> Ok None
        | c when c > 16 -> Error "Flattened extension exceeds 16-character ceiling"
        | _ ->
            let final_str = String.of_seq (List.to_seq (List.rev acc)) in
            Ok (Some final_str))

    | u :: rest ->
        let code = Uchar.to_int u in
        if code < 0 || code > 127 then
          loop acc count rest
        else
          let c = Char.chr code in
          match c with
          | 'a' .. 'z' ->
              loop (Char.uppercase_ascii c :: acc) (count + 1) rest
          | 'A' .. 'Z' | '0' .. '9' ->
              loop (c :: acc) (count + 1) rest
          | _ ->
              loop acc count rest
  in
  loop [] 0 uchars

let extension_unicode_parser raw_s =
  let decoder = Uutf.decoder (`String raw_s) in
  let rec decode_all acc =
    match Uutf.decode decoder with
    | `Await -> Error "Unexpected streaming block"
    | `Malformed _ -> Error "Input contains invalid UTF-8 byte sequences"
    | `End -> extension_parser (List.rev acc) (* remember, acc is reversed! *)
    | `Uchar uchar ->
        if Uchar.to_int uchar = 0 then
          Error "Malicious input: String contains null bytes"
        else
          decode_all (uchar :: acc)
  in
  decode_all []

(* Sometimes, strings have \0 as an artefact from C *)
(* we allow \0 inside the string, but only at the very end *)
(* else it is rejected *)
let validate_and_strip_nulls raw_s =
  let len = String.length raw_s in
  match String.index_from_opt raw_s 0 '\000' with
  | None -> Ok raw_s
  | Some idx when idx = len - 1 -> Ok (String.sub raw_s 0 idx)
  | Some idx -> Error (Printf.sprintf "Security Exception: Embedded null byte detected at position %d" idx)



(* of_string, a public API that handle prefix_unicode_parser crashes,
   then we put the pile of digits into the container, then parse the extension
   raw string and pack it into a 16 character buffer. If the provided extension
   cannot fit to the 16 characters buffer, it will return an error.

   We disallow whitespaces other than " " because here we don't have any context
   of where these whitespaces exist and why do they spawn to existence, thus it is better
   for the caller of the function to figure out the semantics.

   Aka, no custom data forms would accept "23
   3452" (23\n3452) as a valid HS code.
*)
let of_string raw_s =
  let open Result.Syntax in

  let* clean_s = validate_and_strip_nulls raw_s in

  (* First parse the prefix to a 6 digit prefix, then match the error *)
  match prefix_unicode_parser clean_s with
  | Invalid msg -> Error msg
  | ValidPrefix (prefix, raw_ext) ->

      (* Slice the prefix buckets, guaranteed to be exactly
         6 pure ASCII numeric chars by the scanner *)
      let c_raw = String.sub prefix 0 2 in
      let h_raw = String.sub prefix 2 2 in
      let s_raw = String.sub prefix 4 2 in
      let* chapter    = Chapter.of_string c_raw in
      let* heading    = Heading.of_string h_raw in
      let* subheading = Subheading.of_string s_raw in

      (* validate and parse extension *)
      let* extension_opt =
        match extension_unicode_validator raw_ext with
        | Invalid msg -> Error msg
        | ValidPrefix (_, _) ->
            extension_unicode_parser raw_ext
      in
      let* extension =
        match extension_opt with
        | None -> Ok None
        | Some e ->
            let* valid_ext = Extension.of_string e in
            Ok (Some valid_ext)
      in

      Ok { chapter; heading; subheading; extension }

let of_string_exn s = match of_string s with Ok t -> t | Error msg -> failwith msg

let to_string { chapter; heading; subheading; extension } =
  let c = Chapter.to_int chapter in
  let h = Heading.to_int heading in
  let s = Subheading.to_int subheading in
  match extension with
  | None -> Printf.sprintf "%02d.%02d.%02d" c h s
  | Some e ->
      let e_str = Extension.to_string e in
      Printf.sprintf "%02d.%02d.%02d.%s" c h s e_str

let pp ppf t =
  Format.pp_print_string ppf (to_string t)


(* getters *)
let chapter t = Chapter.to_int t.chapter
let heading t = Heading.to_int t.heading
let subheading t = Subheading.to_int t.subheading
let extension t =
  match t.extension with
  | None -> None
  | Some e -> Some (Extension.to_string e)


(* comparison *)
type match_level =
  | Identical
  | Chapter_mismatch
  | Heading_mismatch
  | Subheading_mismatch
  | Extension_mismatch

let match_level a b =
  let c = Chapter.to_int a.chapter = Chapter.to_int b.chapter in
  let h = Heading.to_int a.heading = Heading.to_int b.heading in
  let s = Subheading.to_int a.subheading = Subheading.to_int b.subheading in
  let e = Option.equal String.equal
            (Option.map Extension.to_string a.extension)
            (Option.map Extension.to_string b.extension)
  in

  match (c, h, s, e) with
  | (true,  true,  true,  true)  -> Identical
  | (true,  true,  true,  false) -> Extension_mismatch
  | (true,  true,  false, _)     -> Subheading_mismatch
  | (true,  false, _,     _)     -> Heading_mismatch
  | (false, _,     _,     _)     -> Chapter_mismatch


let compare a b =
  match match_level a b with
  | Identical -> 0
  | Chapter_mismatch ->
      Int.compare (Chapter.to_int a.chapter) (Chapter.to_int b.chapter)
  | Heading_mismatch ->
      Int.compare (Heading.to_int a.heading) (Heading.to_int b.heading)
  | Subheading_mismatch ->
      Int.compare (Subheading.to_int a.subheading) (Subheading.to_int b.subheading)
  | Extension_mismatch ->
      Option.compare String.compare
        (Option.map Extension.to_string a.extension)
        (Option.map Extension.to_string b.extension)

let equal a b =
  match match_level a b with
  | Identical -> true
  | _ -> false
