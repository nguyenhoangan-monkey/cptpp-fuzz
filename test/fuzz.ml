open AflPersistent

let () =
  let run_buf = Bytes.create 65536 in
  AflPersistent.run (fun () ->
    try
      let len = Unix.read Unix.stdin run_buf 0 (Bytes.length run_buf) in
      if len > 0 then
        let input = Bytes.sub_string run_buf 0 len in
        
        match Hs_code.of_string input with
        | Error _ -> () (* Expected parser rejection on malformed mutations *)
        | Ok parsed ->
            (* 1. Verify To_String / Of_String Round-trip Invariant *)
            let serialized = Hs_code.to_string parsed in
            match Hs_code.of_string serialized with
            | Error msg -> 
                failwith ("Failed to parse serialized string: " ^ msg)
            | Ok round_tripped ->
                if not (Hs_code.equal parsed round_tripped) then 
                  failwith "Round-trip equality mismatch!";
                
                (* 2. Exercise Format Pretty Printer *)
                let _ = Format.asprintf "%a" Hs_code.pp parsed in

                (* 3. Exercise Semantic Getters (Watch for unexpected exceptions) *)
                let ch  = Hs_code.chapter parsed in
                let hd  = Hs_code.heading parsed in
                let sub = Hs_code.subheading parsed in
                let ext = Hs_code.extension parsed in

                (* 4. Verify Getter Bounds sanity check against domain logic *)
                if ch < 1 || ch > 99 then failwith "Chapter out of bounds";
                if hd < 0 || hd > 99 then failwith "Heading out of bounds";
                if sub < 0 || sub > 99 then failwith "Subheading out of bounds";
                
                (* FIXED: ext is now just a string, no option matching needed *)
                if String.length ext > 16 then 
                  failwith "Extension violates length ceiling";

                (* 5. Verify Reflexive Comparison Invariant *)
                if Hs_code.compare parsed parsed <> 0 then
                  failwith "Reflexivity failure: compare x x !== 0";
                if not (Hs_code.equal parsed parsed) then
                  failwith "Reflexivity failure: equal x x is false";

                (* 6. Exercise of_string_exn with guaranteed valid input *)
                let _ = Hs_code.of_string_exn serialized in
                ()
    with
    (* Let unhandled system exceptions (like Invalid_argument, Out_of_memory, Stack_overflow) *)
    (* escape to crash the binary so AFL++ can capture and log them. *)
    | Failure msg -> failwith msg
  )