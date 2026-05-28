open Printf

let input_file = "test/seeds.txt"
let output_dir = "input"

(* Helper to strip trailing \r or \n *)
let strip_trailing_newlines s =
  let len = String.length s in
  let rec loop i =
    if i < 0 then ""
    else match s.[i] with
      | '\n' | '\r' -> loop (i - 1)
      | _ -> String.sub s 0 (i + 1)
  in
  loop (len - 1)

(* Clears out all files inside the target directory *)
let clear_output_directory dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    let files = Sys.readdir dir in
    Array.iter (fun file ->
      let full_path = Filename.concat dir file in
      if not (Sys.is_directory full_path) then
        Sys.remove full_path
    ) files

let () =
  (* Clear the directory directly since it is guaranteed to exist *)
  clear_output_directory output_dir;

  try
    let ic = open_in input_file in
    let rec loop count =
      try
        let raw_line = input_line ic in
        let clean_line = strip_trailing_newlines raw_line in
        
        if String.length clean_line > 0 then begin
          (* Updated filename format to append .txt *)
          let out_filename = Filename.concat output_dir (sprintf "seed_%d.txt" count) in
          let oc = open_out_bin out_filename in
          
          output_string oc clean_line;
          close_out oc;
          loop (count + 1)
        end else
          loop count
      with End_of_file ->
        close_in ic;
        printf "Successfully cleaned and split seeds into '%s/' directory.\n" output_dir
    in
    loop 1
  with Sys_error msg ->
    eprintf "Error opening source file: %s\n" msg;
    exit 1