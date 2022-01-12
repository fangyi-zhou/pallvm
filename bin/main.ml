let () =
  let filename = Sys.argv.(1) in
  match Pallvm.parse filename with
  | Some program -> Pallvm.generate_code program
  | None -> ()
