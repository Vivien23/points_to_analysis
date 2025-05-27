let print_fake_read = false;;

let print_fun_names crate =

  let fun_decls_print _ (fun_decl:'fun_body Charon.GAst.gfun_decl) =
    print_endline (Charon.PrintTypes.name_to_string (Charon.PrintUtils.of_crate crate) fun_decl.item_meta.name);
  in
  Charon.Generated_Types.FunDeclId.Map.iter fun_decls_print crate.fun_decls
;;

let print_statements (crate: Charon.UllbcAst.crate) =

  let behaviour_on_statement (statement: Charon.UllbcAst.statement) =
      match statement.content with
      | Assign (place, rvalue) -> 
          let assign_to = 
            match place.kind with
            | PlaceBase var_id -> Charon.PrintExpressions.var_id_to_pretty_string var_id
            | PlaceProjection (_place, _proj_elem) -> "TODO : projections"
          in 
          let assign_from =
            match rvalue with
            | Use _ -> "Use"
            | RvRef _ -> "RvRef"
            | RawPtr _ -> "omg raw ptr"
            | BinaryOp _ -> "BinOp"
            | UnaryOp _ -> "UnaOp"
            | NullaryOp _ -> "NulOp"
            | Discriminant _ -> "What is a discriminant"
            | Aggregate _ -> "omg aggregate"
            | Global global_decl_ref -> Charon.PrintGAst.any_decl_id_to_string (IdGlobal global_decl_ref.global_id)
            | _ -> "Catch all case"
          in print_string "Assign "; print_string assign_from; print_string " to "; print_endline assign_to
      | Call _ -> print_endline "Call"
      | FakeRead _ -> if print_fake_read then print_endline "FakeRead"
      | SetDiscriminant _ -> print_endline "SetDiscriminant"
      | StorageDead _ -> print_endline "StorageDead"
      | Deinit _ -> print_endline "Deinit"
      | Drop _ -> print_endline "Drop"
      | Assert _ -> print_endline "Assert"
      | Nop -> print_endline "Nop"
  in

  let behaviour_on_fun_decl _ (fun_decl: Charon.UllbcAst.blocks Charon.UllbcAst.gfun_decl) =
    match fun_decl.body with
    | Some(bdy: Charon.UllbcAst.blocks Charon.UllbcAst.gexpr_body) -> List.iter (fun (block: Charon.UllbcAst.block)  -> print_endline "Entering new block :"; List.iter behaviour_on_statement block.statements; print_endline "") (bdy.body)
    | None -> ()
  in 

  Charon.Types.FunDeclId.Map.iter behaviour_on_fun_decl crate.fun_decls
;;  
