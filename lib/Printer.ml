
let print_fun_names filename =
  let crate = GetAST.load_file filename in

  let fun_decls_print _ (fun_decl:'fun_body Charon.GAst.gfun_decl) =
    print_endline (Charon.PrintTypes.name_to_string (Charon.PrintUtils.of_crate crate) fun_decl.item_meta.name);
  in
  Charon.Generated_Types.FunDeclId.Map.iter fun_decls_print crate.fun_decls
;;

let print_statements filename =
  let crate = GetAST.load_file filename in 

  let behaviour_on_statement () (statement: Charon.Generated_UllbcAst.statement) =
      match statement.content with
      | Assign _ -> ()
      | Call _ -> ()
      | FakeRead _ -> ()
      | SetDiscriminant _ -> ()
      | StorageDead _ -> ()
      | Deinit _ -> ()
      | Drop _ -> ()
      | Assert _ -> ()
      | Nop -> ()
  in

  let behaviour_on_fun_decl _ (fun_decl: Charon.UllbcAst.blocks Charon.UllbcAst.gfun_decl) =
    match fun_decl.body with
    | Some(bdy: Charon.UllbcAst.blocks Charon.UllbcAst.gexpr_body) -> List.fold_left (fun _ (block: Charon.UllbcAst.block)  -> List.fold_left behaviour_on_statement () block.statements) () (bdy.body)
    | None -> ()
  in 

  Charon.Generated_Types.FunDeclId.Map.iter behaviour_on_fun_decl crate.fun_decls
;;  
