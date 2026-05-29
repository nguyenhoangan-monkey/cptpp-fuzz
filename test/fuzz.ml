open AflPersistent

let () =
  let run_buf = Bytes.create 65536 in
  AflPersistent.run (fun () ->
    try
      let len = Unix.read Unix.stdin run_buf 0 (Bytes.length run_buf) in
      if len > 0 then
        let input = Bytes.sub_string run_buf 0 len in
        
        match Hs_code.of_string input with
        | Error _ -> ()
        | Ok parsed ->
            let serialized = Hs_code.to_string parsed in
            match Hs_code.of_string serialized with
            | Error msg -> 
                failwith ("Failed to parse serialized string: " ^ msg)
            | Ok round_tripped ->
                if parsed <> round_tripped then 
                  failwith "Round-trip mismatch!"
    with
    | Failure msg -> failwith msg
  )