
let printf_fun_names () =
  let crate = GetAST.load_file "test/own_cf.ullbc" in

  let fun_decls_print _ (fun_decl:'fun_body Charon.GAst.gfun_decl) =
    print_endline (Charon.PrintTypes.name_to_string (Charon.PrintUtils.of_crate crate) fun_decl.item_meta.name);
  in
  Charon.Generated_Types.FunDeclId.Map.iter fun_decls_print crate.fun_decls
;;
