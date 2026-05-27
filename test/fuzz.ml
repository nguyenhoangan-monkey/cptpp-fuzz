open AflPersistent

let () =
  AflPersistent.run (fun () ->
    if Array.length Sys.argv < 2 then exit 1;
    
    let filename = Sys.argv.(1) in
    try
      let ic = open_in filename in
      let len = in_channel_length ic in
      let input = really_input_string ic len in
      close_in ic;

      let _ = Hs_code.of_string input in
      ()
    with
    | Failure _ -> () 
  )
