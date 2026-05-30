(* HS CODE FUNCTORS AND TYPES *)
module type Bounded_int_config = sig
  val min : int
  val max : int
  val name : string
end

(* Here we parse the 2 char string, check digit, and manually parse digit to int *)
(* Note: single digit printed and string are explicitly disallowed in HS code *)
module Bounded_int (Config : Bounded_int_config) : sig
  type t

  val of_string : string -> (t, string) result
  val to_int : t -> int
end = struct
  type t = int

  let of_string s =
    match String.length s with
    | 2 -> (
        match (s.[0], s.[1]) with
        | ('0' .. '9' as c1), ('0' .. '9' as c2) ->
            let n = ((int_of_char c1 - 48) * 10) + (int_of_char c2 - 48) in
            if n >= Config.min && n <= Config.max then Ok n
            else
              Error
                (Printf.sprintf "%s value %02d is out of bounds (must be between %02d and %02d)" Config.name n
                   Config.min Config.max)
        | _, _ -> Error (Printf.sprintf "Invalid character in %s segment; must be a digit" Config.name))
    | _ -> Error (Printf.sprintf "%s segment must be exactly 2 characters long (received %S)" Config.name s)

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

  let is_valid_char = function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

  let of_string s =
    match String.length s with
    | 0 -> Ok s
    | len when len > 16 -> Error "Extension exceeded 16 characters"
    | _ ->
        if String.for_all is_valid_char s then Ok s
        else Error "Extension contains invalid characters (must be uppercase alphanumeric)"

  let to_string t = t
end

type t = { chapter : Chapter.t; heading : Heading.t; subheading : Subheading.t; extension : Extension.t }

(* HS CODE PREFIX (12.34.56) PARSING *)
(* chunk denote digits next to each other *)
module Chunk = struct
  type t = C1 | C2 | C4 | C6 | Space | Dash | Slash | Dot
end

type prefix_token = Digits of string | Delims of string

(* Consume exactly up to 6 digits from the input stream. It groups consecutive digits
into strings, validates and skips delimiters, and immediately stops when it has seen
6 digits, returning the unconsumed rest of the stream *)
let tokenizer unicode_stream =
  (* converts char list to string, *)
  (* wraps it in the correct token type, and appends to list *)
  let flush state chars acc_list =
    match chars with
    | [] -> acc_list
    | _ -> (
        let str = chars |> List.rev |> List.to_seq |> String.of_seq in
        match state with `Digits -> Digits str :: acc_list | `Delims -> Delims str :: acc_list | `None -> acc_list)
  in

  let rec loop acc_list current_chars state digits_needed stream =
    match digits_needed with
    | 0 ->
        let final_blocks = flush state current_chars acc_list in
        Ok (List.rev final_blocks, stream)
    | _ -> (
        match stream with
        | [] -> Error "Input ended before 6 digits were collected"
        | u :: rest -> (
            match Uchar.to_char u with
            | '0' .. '9' as d -> (
                match state with
                | `Digits | `None -> loop acc_list (d :: current_chars) `Digits (digits_needed - 1) rest
                | `Delims ->
                    let new_list = flush state current_chars acc_list in
                    loop new_list [ d ] `Digits (digits_needed - 1) rest)
            | (' ' | '_' | '-' | '.' | '/') as c -> (
                match state with
                | `Delims | `None -> loop acc_list (c :: current_chars) `Delims digits_needed rest
                | `Digits ->
                    let new_list = flush state current_chars acc_list in
                    loop new_list [ c ] `Delims digits_needed rest)
            | _ -> Error "Illegal character in prefix"))
  in
  loop [] [] `None 6 unicode_stream

(* Classify other strings as delimiter tokens *)
let chunks_of_tokens tokens =
  let chunk_of_int = function
    | 1 -> Ok Chunk.C1
    | 2 -> Ok Chunk.C2
    | 4 -> Ok Chunk.C4
    | 6 -> Ok Chunk.C6
    | n -> Error (Printf.sprintf "Invalid digit block size: %d" n)
  in

  let classify_delims str =
    let spaces, dashes, underscores, dots, slashes =
      String.fold_left
        (fun (s, d, u, pt, sl) -> function
          | ' ' -> (s + 1, d, u, pt, sl)
          | '-' -> (s, d + 1, u, pt, sl)
          | '_' -> (s, d, u + 1, pt, sl)
          | '.' -> (s, d, u, pt + 1, sl)
          | '/' -> (s, d, u, pt, sl + 1)
          | _ -> (s, d, u, pt, sl))
        (0, 0, 0, 0, 0) str
    in

    (* structural validations, then check count, then pattern match *)
    (* notice that dot and slash count is hardcoded as 1 *)
    if dots > 1 || slashes > 1 then Error "Illegal sequence: Consecutive or multiple '.' or '/'"
    else if dots > 0 && slashes > 0 then Error "Illegal sequence: Cannot mix '.' and '/'"
    else if dashes > 0 && underscores > 0 then Error "Illegal sequence: Cannot mix '-' and '_'"
    else if (dashes > 0 || underscores > 0) && (dots > 0 || slashes > 0) then
      Error "Illegal sequence: Cannot mix dashes/underscores with '.' or '/'"
    else if (dashes > 0 || underscores > 0) && (dots > 0 || slashes > 0) then
      Error "Illegal sequence: Cannot mix dashes/underscores with '.' or '/'"
    else if spaces > 6 then Error "Illegal sequence: Too many space characters (more than 6)"
    else if dashes > 3 then Error "Illegal sequence: Too many dashes (more than 3)"
    else if underscores > 3 then Error "Illegal sequence: Too many underscores (more than 3)"
    else
      match (dashes, underscores, dots, slashes) with
      | 0, 0, 0, 0 -> Ok Chunk.Space
      | 0, 0, 1, 0 -> Ok Chunk.Dot
      | 0, 0, 0, 1 -> Ok Chunk.Slash
      | _, 0, 0, 0 -> Ok Chunk.Dash
      | 0, _, 0, 0 -> Ok Chunk.Dash
      | _ -> Error "Unhandled sequence state"
  in

  let open Result.Syntax in
  let rec loop acc remaining_tokens =
    match remaining_tokens with
    | [] -> Ok (List.rev acc)
    | token :: rest ->
        let* next_chunk =
          match token with Digits d -> chunk_of_int (String.length d) | Delims c -> classify_delims c
        in
        loop (next_chunk :: acc) rest
  in
  loop [] tokens

let extract_prefix chunks prefix_tokens =
  (* strip delimiters *)
  let digit_strings = List.filter_map (function Digits s -> Some s | Delims _ -> None) prefix_tokens in

  (* Essentially is a list of all acceptable formats *)
  let open Result.Syntax in
  let* c_raw, h_raw, s_raw =
    let open Chunk in
    match (chunks, digit_strings) with
    (* [2; 2; 2] *)
    | [ C2; _; C2; _; C2 ], [ c; h; s ] -> Ok (c, h, s)
    (* [4; 2] *)
    | [ C4; _; C2 ], [ ch; s ] -> Ok (String.sub ch 0 2, String.sub ch 2 2, s)
    (* [6] *)
    | [ C6 ], [ chs ] -> Ok (String.sub chs 0 2, String.sub chs 2 2, String.sub chs 4 2)
    (* [1; 1; 1; 1; 1; 1] *)
    | [ C1; Space; C1; _; C1; Space; C1; _; C1; Space; C1 ], [ c1; c2; h1; h2; s1; s2 ] -> Ok (c1 ^ c2, h1 ^ h2, s1 ^ s2)
    (* [1; 1; 1; 1; 2] *)
    | [ C1; Space; C1; _; C1; Space; C1; _; C2 ], [ c1; c2; h1; h2; s ] -> Ok (c1 ^ c2, h1 ^ h2, s)
    (* [1; 1; 2; 1; 1] *)
    | [ C1; Space; C1; _; C2; _; C1; Space; C1 ], [ c1; c2; h; s1; s2 ] -> Ok (c1 ^ c2, h, s1 ^ s2)
    (* [1; 1; 2; 2] *)
    | [ C1; Space; C1; _; C2; _; C2 ], [ c1; c2; h; s ] -> Ok (c1 ^ c2, h, s)
    (* [1; 1; 4] *)
    | [ C1; Space; C1; _; C4 ], [ c1; c2; hs ] -> Ok (c1 ^ c2, String.sub hs 0 2, String.sub hs 2 2)
    (* [2; 1; 1; 1; 1] *)
    | [ C2; _; C1; Space; C1; _; C1; Space; C1 ], [ c; h1; h2; s1; s2 ] -> Ok (c, h1 ^ h2, s1 ^ s2)
    (* [2; 1; 1; 2] *)
    | [ C2; _; C1; Space; C1; _; C2 ], [ c; h1; h2; s ] -> Ok (c, h1 ^ h2, s)
    (* [2; 2; 1; 1] *)
    | [ C2; _; C2; _; C1; Space; C1 ], [ c; h; s1; s2 ] -> Ok (c, h, s1 ^ s2)
    (* [4; 1; 1] *)
    | [ C4; _; C1; Space; C1 ], [ ch; s1; s2 ] -> Ok (String.sub ch 0 2, String.sub ch 2 2, s1 ^ s2)
    (* CATCH-ALL FOR MALFORMED PATTERNS, verified with knapsack problem
      (*              
      [1; 1; 1; 2; 1]
      [1; 2; 1; 1; 1]
      [1; 2; 1; 2]
      [1; 2; 2; 1]
      [1; 4; 1]
      [2; 1; 2; 1]
      [2; 4] *)
    *)
    | _ -> Error "Layout configuration is mathematically forbidden or incorrectly sized"
  in

  let* chapter = Chapter.of_string c_raw in
  let* heading = Heading.of_string h_raw in
  let* subheading = Subheading.of_string s_raw in
  Ok (chapter, heading, subheading)

(* HS CODE EXTENSION (12.34.56-789AB) PARSING *)
type bracket_context = Outside | Inside of char * bool

(* Now, we clean and check whether it has any semantic meaning *)
(* The caller has the responsibility to enforce data hierarchy. *)
(* because of it, we use a state machine. *)
let clean_of_tokens uchars =
  let rec loop ctx streak = function
    | [] -> if ctx = Outside then Ok uchars else Error "Unclosed bracket at end"
    | u :: rest -> (
        let c = Uchar.to_char u in
        match c with
        | ' ' -> loop ctx streak rest
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' ->
            let next_ctx = match ctx with Inside (c, _) -> Inside (c, true) | Outside -> Outside in
            loop next_ctx None rest
        | ('[' | '(') as c ->
            if ctx <> Outside then Error "Nested brackets not allowed"
            else loop (Inside ((if c = '[' then ']' else ')'), false)) None rest
        | (']' | ')') as c -> (
            match ctx with
            | Inside (exp, true) when c = exp -> loop Outside None rest
            | Inside (_, false) -> Error "Bracket contains no alphanumeric characters"
            | _ -> Error "Mismatched or unmatched closing bracket")
        | ('-' | '/' | ':' | '_' | '.') as c ->
            if streak <> None && streak <> Some c then Error "Heterogeneous continuous delimiters detected"
            else loop ctx (Some c) rest
        | c -> Error (Printf.sprintf "Invalid character '%c' in extension" c))
  in
  loop Outside None uchars

(* here we flatten the implicit hierarchy of the extension for the densest *)
(* most compact representation, as recommended by the *)
(* Data Element 7357 / TDED 7357 (WCO Data Model). *)
let extension_of_tokens uchars =
  let buf = Buffer.create 16 in

  let rec loop chars =
    if Buffer.length buf > 16 then Error "Flattened extension exceeds 16-character ceiling"
    else
      match chars with
      | [] -> Ok (Buffer.contents buf)
      | u :: rest ->
          if Uchar.is_char u then
            match Uchar.to_char u with
            | 'a' .. 'z' as c ->
                Buffer.add_char buf (Char.uppercase_ascii c);
                loop rest
            | ('A' .. 'Z' | '0' .. '9') as c ->
                Buffer.add_char buf c;
                loop rest
            | ' ' | '[' | ']' | '(' | ')' | '-' | '/' | ':' | '_' | '.' -> loop rest
            | c -> Error (Printf.sprintf "Unexpected validation leak: illegal character '%c'" c)
          else Error "Unexpected validation leak: non-ASCII unicode character encountered"
  in
  loop uchars

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
  (* Guards and preprocessing *)
  (* 1. no strings longer than >128 characters *)
  (* 2. allow \0 inside the string, but only at the very end *)
  (* 3. Turn raw string into UTF-8, disallow multi-byte chars *)
  let open Result.Syntax in
  let* () =
    if String.length raw_s > 128 then Error "Input string length exceeds the maximum allowable limit of 128 characters"
    else Ok ()
  in

  let* clean_s =
    match String.index_from_opt raw_s 0 '\000' with
    | None -> Ok raw_s
    | Some idx ->
        if idx = String.length raw_s - 1 then Ok (String.sub raw_s 0 idx)
        else Error (Printf.sprintf "Security Exception: Embedded null byte detected at position %d" idx)
  in

  let* input_str =
    let decoder = Uutf.decoder (`String clean_s) in
    let rec decode_all acc =
      match Uutf.decode decoder with
      | `Await -> Error "Unexpected streaming block"
      | `Malformed _ -> Error "Input contains invalid UTF-8 byte sequences"
      | `End -> Ok (List.rev acc)
      | `Uchar uchar ->
          let code = Uchar.to_int uchar in
          if code > 127 then Error "Non-ASCII characters are not allowed" else decode_all (uchar :: acc)
    in
    decode_all []
  in

  let* prefix_tokens, remaining_tokens = tokenizer input_str in

  (* Consume stream to get prefix *)
  let* prefix_chunks = chunks_of_tokens prefix_tokens in
  let* chapter, heading, subheading = extract_prefix prefix_chunks prefix_tokens in

  (* Consume stream to get extension *)
  let* clean_ext_tokens = clean_of_tokens remaining_tokens in
  let* extension_opt = extension_of_tokens clean_ext_tokens in
  let* extension = Extension.of_string extension_opt in

  Ok { chapter; heading; subheading; extension }

(* OTHER USEFUL HS CODE HELPERS *)
(* string-related operations *)
let of_string_exn s = match of_string s with Ok t -> t | Error msg -> failwith msg

let to_string { chapter; heading; subheading; extension } =
  let c = Chapter.to_int chapter in
  let h = Heading.to_int heading in
  let s = Subheading.to_int subheading in
  let e = Extension.to_string extension in
  match e with "" -> Printf.sprintf "%02d.%02d.%02d" c h s | _ -> Printf.sprintf "%02d.%02d.%02d-%s" c h s e

let pp ppf t = Format.pp_print_string ppf (to_string t)

(* getters *)
let chapter t = Chapter.to_int t.chapter
let heading t = Heading.to_int t.heading
let subheading t = Subheading.to_int t.subheading
let extension t = Extension.to_string t.extension

(* comparison *)
let compare a b =
  let res = Int.compare (Chapter.to_int a.chapter) (Chapter.to_int b.chapter) in
  if res <> 0 then res
  else
    let res = Int.compare (Heading.to_int a.heading) (Heading.to_int b.heading) in
    if res <> 0 then res
    else
      let res = Int.compare (Subheading.to_int a.subheading) (Subheading.to_int b.subheading) in
      if res <> 0 then res else String.compare (Extension.to_string a.extension) (Extension.to_string b.extension)

let equal a b = compare a b = 0
