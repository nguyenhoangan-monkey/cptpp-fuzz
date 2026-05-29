open AflPersistent

let () =
  (* Allocate a single large, reusable buffer outside the loop to maximize speed *)
  let run_buf = Bytes.create 65536 in
  
  AflPersistent.run (fun () ->
    try
      (* Execute a raw, unbuffered system read directly from file descriptor 0 *)
      let len = Unix.read Unix.stdin run_buf 0 (Bytes.length run_buf) in
      if len > 0 then
        let input = Bytes.sub_string run_buf 0 len in
        (* Test your pure function with the exact raw mutation payload *)
        let _ = Hs_code.of_string input in
        ()
    with
    | _ -> () 
  )
  