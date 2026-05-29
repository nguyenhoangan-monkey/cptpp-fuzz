open AflPersistent

let () =
  let run_buf = Bytes.create 65536 in
  AflPersistent.run (fun () ->
    try
      let len = Unix.read Unix.stdin run_buf 0 (Bytes.length run_buf) in
      if len > 0 then
        let input = Bytes.sub_string run_buf 0 len in
        
        let _ =
          let open Result.Syntax in
          let* parsed = Hs_code.of_string input in
          let serialized = Hs_code.to_string parsed in
          let* round_tripped = Hs_code.of_string serialized in
          
          if parsed <> round_tripped then 
            failwith "Round-trip mismatch"
          else 
            Ok ()
        in
        ()
    with
    | Failure msg -> failwith msg
    | _ -> () 
  )