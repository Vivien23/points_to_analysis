let load_file filename =
  let json = Yojson.Basic.from_file filename in
  match Charon.UllbcOfJson.crate_of_json json with
  | Ok r -> r
  | Error e ->
      Printf.fprintf stderr
        "Error loading JSON. This is typically due to a discrepancy between charon-ml and charon. \
         See error below. (Search for \"failed on\"). \n\n\
         %s"
        e;
      exit 1

  
