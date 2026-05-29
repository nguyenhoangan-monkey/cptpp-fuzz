(***** HS CODE FUNCTORS AND TYPES ******)
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
    if String.length s <> 2 then
      Error (Printf.sprintf "%s segment must be exactly 2 characters long (received %S)" Config.name s)
    else
      match s.[0], s.[1] with
      | ('0'..'9' as c1), ('0'..'9' as c2) ->
          let n = (int_of_char c1 - 48) * 10 + (int_of_char c2 - 48) in
          if n >= Config.min && n <= Config.max then Ok n
          else Error (Printf.sprintf "%s value %02d is out of bounds (must be between %02d and %02d)"
                     Config.name n Config.min Config.max)

      | _, _ ->
          Error (Printf.sprintf "Invalid character in %s segment; must be a digit" Config.name)

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
    | 0 -> Error "Extension cannot be empty"
    | len when len > 16 -> Error "Extension exceeded 16 characters"
    | _ ->
        if String.for_all is_valid_char s then Ok s
        else Error "Extension contains invalid characters (must be uppercase alphanumeric)"

  let to_string t = t
end

type t = { chapter : Chapter.t; heading : Heading.t; subheading : Subheading.t; extension : Extension.t option }


(***** HS CODE PREFIX (12.34.56) PARSING ******)
(* chunk denote digits next to each other *)
module Chunk = struct
  type t = C1 | C2 | C4 | C6 | Space | Dash | Slash_dot

  let of_int = function
    | 1 -> Ok C1 | 2 -> Ok C2 | 4 -> Ok C4 | 6 -> Ok C6
    | n -> Error (Printf.sprintf "Invalid digit block size: %d" n)
end

type prefix_token = 
  | Digits of string
  | Delims of string

(* Consume exactly up to 6 digits from the input stream. It groups consecutive digits
into strings, validates and skips delimiters, and immediately stops when it has seen
6 digits, returning the unconsumed rest of the stream *)
let tokens_of_unicode unicode_stream =
  (* converts char list to string, *)
  (* wraps it in the correct token type, and appends to list *)
  let flush state chars acc_list =
    if chars = [] then acc_list
    else
      let str = chars |> List.rev |> List.to_seq |> String.of_seq in
      match state with
      | `Digits -> Digits str :: acc_list
      | `Delims -> Delims str :: acc_list
      | `None   -> acc_list
  in

  let rec loop acc_list current_chars state digits_needed stream =
    match digits_needed with
    | 0 ->
      let final_blocks = flush state current_chars acc_list in
      Ok (List.rev final_blocks, stream)
    | _ ->
      match stream with
      | [] -> Error "Input ended before 6 digits were collected"
      | u :: rest ->
          let code = Uchar.to_int u in
          if code > 127 then Error "Multi-byte/Non-ASCII characters are not allowed"
          else match Uchar.to_char u with
          | '0'..'9' as d -> 
              (match state with
              | `Digits | `None ->
                  loop acc_list (d :: current_chars) `Digits (digits_needed - 1) rest
              | `Delims ->
                  let new_list = flush state current_chars acc_list in
                  loop new_list [d] `Digits (digits_needed - 1) rest)
                
          | ' ' | '_' | '-' | '.' | '/' as c ->
              (match state with
              | `Delims | `None ->
                  loop acc_list (c :: current_chars) `Delims digits_needed rest
              | `Digits ->
                  let new_list = flush state current_chars acc_list in
                  loop new_list [c] `Delims digits_needed rest)
                
          | _ -> Error "Illegal character in prefix"
  in
  loop [] [] `None 6 unicode_stream

(* Classify other strings as delimiter tokens *)
let classify_delims str =
  let count c =
    let rec loop acc i =
      (* If we've reached the end of the string, return our accumulated count *)
      if i >= String.length str then 
        acc
      else
        (* Check the character at the current index *)
        match str.[i] with
        | x when x = c -> loop (acc + 1) (i + 1)
        | _            -> loop acc       (i + 1)
    in
    loop 0 0
  in

  match (count ' ', count '-', count '_', count '.' + count '/') with
  | (_, 0, 0, 0) -> Ok Chunk.Space
  | (0, 0, 0, 1) -> Ok Chunk.Slash_dot
  | (0, d, 0, 0) when d > 0 -> Ok Chunk.Dash
  | (0, 0, u, 0) when u > 0 -> Ok Chunk.Dash
  | (_, d, u, _) when d > 0 && u > 0 -> Error "Illegal sequence: Cannot mix '-' and '_'"
  | (_, _, _, dot) when dot > 1 -> Error "Illegal sequence: Consecutive or multiple '.' or '/'"
  | _ -> Error "Illegal sequence: Cannot mix dashes with '.' or '/'"

let chunks_of_tokens tokens =
  let open Result.Syntax in
  let rec loop acc remaining_tokens =
    match remaining_tokens with
    | [] -> Ok (List.rev acc)
    | token :: rest ->
        let* next_chunk = 
          match token with
          | Digits d -> 
              (* Map the length of the string to a Chunk (C1, C2, C4, C6) *)
              Chunk.of_int (String.length d)
          | Delims c -> 
              (* Apply our strict delimiter classification rules *)
              classify_delims c
        in
        loop (next_chunk :: acc) rest
  in
  loop [] tokens

(* Essentially is a list of all acceptable formats *)
(* Aka this is a compiler *)
let prefix_of_chunks chunks digit_strings =
  let open Result.Syntax in
  
  match chunks, digit_strings with
  (* 1 2 3 4 5 6 *)
  | [Chunk.C1; _; Chunk.C1; _; Chunk.C1; _; Chunk.C1; _; Chunk.C1; _; Chunk.C1], [c1; c2; h1; h2; s1; s2] ->
      let* chapter    = Chapter.of_string (c1 ^ c2) in
      let* heading    = Heading.of_string (h1 ^ h2) in
      let* subheading = Subheading.of_string (s1 ^ s2) in
      Ok (chapter, heading, subheading)

  (* 12-34-56 *)
  | [Chunk.C2; Chunk.Dash; Chunk.C2; Chunk.Dash; Chunk.C2], [c_str; h_str; s_str] ->
      let* chapter    = Chapter.of_string c_str in
      let* heading    = Heading.of_string h_str in
      let* subheading = Subheading.of_string s_str in
      Ok (chapter, heading, subheading)

  (* 1234.56 *)
  | [Chunk.C4; Chunk.Slash_dot; Chunk.C2], [c_str; s_str] ->
      let c_raw = String.sub c_str 0 2 in
      let h_raw = String.sub c_str 2 2 in
      let* chapter    = Chapter.of_string c_raw in
      let* heading    = Heading.of_string h_raw in
      let* subheading = Subheading.of_string s_str in
      Ok (chapter, heading, subheading)

  (* 123456 *)
  | [Chunk.C6], [c_str] ->
      let c_raw = String.sub c_str 0 2 in
      let h_raw = String.sub c_str 2 2 in
      let s_raw = String.sub c_str 4 2 in
      let* chapter    = Chapter.of_string c_raw in
      let* heading    = Heading.of_string h_raw in
      let* subheading = Subheading.of_string s_raw in
      Ok (chapter, heading, subheading)

  | _ -> 
      Error "Layout configuration is mathematically forbidden or incorrectly sized"
  


(***** HS CODE EXTENSION (12.34.56-789AB) PARSING ******)
type bracket_context =
| Outside
| Inside of char * bool

(* Now, we clean and check whether it has any semantic meaning *)
(* The caller has the responsibility to enforce data hierarchy. *)
(* because of it, we use a state machine. *)
let clean_of_tokens uchars =
  let rec loop ctx streak = function
    | [] -> if ctx = Outside then Ok uchars else Error "Unclosed bracket at end"
    | u :: rest ->
        let code = Uchar.to_int u in
        if code > 127 then Error "Non-ASCII characters not allowed"
        else match Char.chr code with
        | ' ' -> loop ctx streak rest
        | 'A'..'Z' | 'a'..'z' | '0'..'9' -> 
            let next_ctx = match ctx with Inside (c, _) -> Inside (c, true) | Outside -> Outside in
            loop next_ctx None rest
        | ('[' | '(') as c -> 
            if ctx <> Outside then Error "Nested brackets not allowed" 
            else loop (Inside ((if c = '[' then ']' else ')'), false)) None rest
        | (']' | ')') as c ->
            (match ctx with
             | Inside (exp, true) when c = exp -> loop Outside None rest
             | Inside (_, false) -> Error "Bracket contains no alphanumeric characters"
             | _ -> Error "Mismatched or unmatched closing bracket")
        | ('-' | '/' | ':' | '_' | '.') as c ->
            if Option.fold ~none:false ~some:(fun last -> c <> last) streak then 
              Error "Heterogeneous continuous delimiters detected"
            else loop ctx (Some c) rest
        | c -> Error (Printf.sprintf "Invalid character '%c' in extension" c)
  in
  loop Outside None uchars


(* here we flatten the implicit hierarchy of the extension for the densest *)
(* most compact representation, as recommended by the *)
(* Data Element 7357 / TDED 7357 (WCO Data Model). *)
let extension_of_tokens uchars =
  (* acc holds the characters in reverse order, count tracks length dynamically *)
  let rec loop acc count chars =
    match chars with
    | [] ->
        begin match count with
        | 0 -> Ok None
        | c when c > 16 -> Error "Flattened extension exceeds 16-character ceiling"
        | _ -> Ok (Some (String.of_seq (List.to_seq (List.rev acc))))
        end

    | u :: rest ->
        let c = Char.chr (Uchar.to_int u) in
        match c with
        | 'a' .. 'z' ->
            loop (Char.uppercase_ascii c :: acc) (count + 1) rest
        | 'A' .. 'Z' | '0' .. '9' ->
            loop (c :: acc) (count + 1) rest
        | ' ' | '[' | ']' | '(' | ')' | '-' | '/' | ':' | '_' | '.' ->
            loop acc count rest
        | _ ->
            Error (Printf.sprintf "Unexpected validation leak: illegal character '%c'" c)
  in
  loop [] 0 uchars



(* of_string, a public API that handle prefix_unicode_parser crashes,
   then we put the pile of digits into the container, then parse the extension
   raw string and pack it into a 16 character buffer. If the provided extension
   cannot fit to the 16 characters buffer, it will return an error.

   We disallow whitespaces other than " " because here we don't have any context
   of where these whitespaces exist and why do they spawn to existence, thus it is better
   for the caller of the function to figure out the semantics.

   Aka, no custom data forms would accept "23
   3452" (23\n3452) as a valid HS code.

   Thus I added two guards: one for string length, one for null bytes.
   Then I parse it to unicode tokens to consume.
*)
let of_string raw_s =
  (***** Guards and preprocessing *****)
  (* 1. no strings longer than >128 characters *)
  (* 2. allow \0 inside the string, but only at the very end *)
  (* 3. Turn raw string into an atomic UTF-8 token stream *)
  let open Result.Syntax in

  let* () =
    if String.length raw_s > 128 then
      Error "Input string length exceeds the maximum allowable limit of 128 characters"
    else Ok ()
  in

  let* clean_s =
    match String.index_from_opt raw_s 0 '\000' with
    | None -> Ok raw_s
    | Some idx when idx = String.length raw_s - 1 -> Ok (String.sub raw_s 0 idx)
    | Some idx -> 
        Error (Printf.sprintf "Security Exception: Embedded null byte detected at position %d" idx)
  in

  let* input_str = 
    let decoder = Uutf.decoder (`String clean_s) in
    let rec decode_all acc =
      match Uutf.decode decoder with
      | `Await -> Error "Unexpected streaming block"
      | `Malformed _ -> Error "Input contains invalid UTF-8 byte sequences"
      | `End -> Ok (List.rev acc)
      | `Uchar uchar -> decode_all (uchar :: acc)
    in
    decode_all []
  in

  (***** Tokenizer *****)
  let* (prefix_tokens, remaining_tokens) = tokens_of_unicode input_str in

  (***** Consume stream to get prefix *****)
  let* chunks = chunks_of_tokens prefix_tokens in
  let digit_strings = 
    List.filter_map (function Digits s -> Some s | Delims _ -> None) prefix_tokens 
  in
  let* (chapter, heading, subheading) = prefix_of_chunks chunks digit_strings in

  (***** Consume stream to get extension *****)
  let* clean_ext_tokens = clean_of_tokens remaining_tokens in
  let* extension_opt    = extension_of_tokens clean_ext_tokens in
  let* extension =
    match extension_opt with
    | None -> Ok None
    | Some e ->
        let* valid_ext = Extension.of_string e in
        Ok (Some valid_ext)
  in

  (***** Return *****)
  Ok { chapter; heading; subheading; extension }


(***** OTHER USEFUL HS CODE HELPERS ******)
(* string-related operations *)
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
let compare a b =
  match Int.compare (Chapter.to_int a.chapter) (Chapter.to_int b.chapter) with
  | 0 -> (
      match Int.compare (Heading.to_int a.heading) (Heading.to_int b.heading) with
      | 0 -> (
          match Int.compare (Subheading.to_int a.subheading) (Subheading.to_int b.subheading) with
          | 0 -> Option.compare String.compare 
                   (Option.map Extension.to_string a.extension) 
                   (Option.map Extension.to_string b.extension)
          | res -> res)
      | res -> res)
  | res -> res

let equal a b = compare a b = 0
