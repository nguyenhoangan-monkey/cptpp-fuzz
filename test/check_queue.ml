let read_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

(* Pass the two output channels as arguments *)
let check_file ch_pass ch_fail filepath =
  let raw_input = read_file filepath in
  match Hs_code.of_string raw_input with
  | Ok record ->
      Printf.fprintf ch_pass "[PASS] File: %s\n" filepath;
      Printf.fprintf ch_pass "  Input : %S\n" raw_input;
      Printf.fprintf ch_pass "  Parsed: %s\n\n" (Hs_code.to_string record)
  | Error msg ->
      Printf.fprintf ch_fail "[FAIL] File: %s\n" filepath;
      Printf.fprintf ch_fail "  Input : %S\n" raw_input;
      Printf.fprintf ch_fail "  Reason: %s\n\n" msg

(* Pass channels through the directory recursion *)
let rec process_dir ch_pass ch_fail path =
  if Sys.file_exists path && Sys.is_directory path then
    let entries = Sys.readdir path in
    Array.fast_sort String.compare entries;
    Array.iter (fun entry ->
      let full_path = Filename.concat path entry in
      if Sys.is_directory full_path then
        process_dir ch_pass ch_fail full_path
      else if String.length entry >= 2 && String.sub entry 0 2 = "id" then
        check_file ch_pass ch_fail full_path
    ) entries

let () =
  let target_dir = "output" in 
  if Sys.file_exists target_dir && Sys.is_directory target_dir then begin
    (* Open the separate output files in text write mode *)
    let ch_pass = open_out "output_pass.txt" in
    let ch_fail = open_out "output_fail.txt" in
    
    (* Process files and write directly to the channels *)
    process_dir ch_pass ch_fail target_dir;
    
    (* Make sure to close the channels to flush buffers to disk *)
    close_out ch_pass;
    close_out ch_fail
  end else
    Printf.eprintf "Error: '%s' directory not found. Run the fuzzer first.\n" target_dir