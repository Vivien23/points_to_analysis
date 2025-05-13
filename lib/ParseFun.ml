
let print_ast () =
  let crate = GetAST.load_file "test/own_cf.llbc" in
  let _ = print_string (Charon.PrintLlbcAst.Crate.crate_to_string crate) in
  ()
;;
