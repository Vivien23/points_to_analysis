open Charon.Expressions
exception Plonk

(* Question : quelle est la différence entre Charon.Type.VarId et Charon.Expression.VarId ? *)
module VarIdMap = Map.Make(struct
  type t = VarId.id
  let compare = VarId.compare_id 
end)

module ProgramPoint = struct
  type block_id = int
  type statement_id = int
  type program_point_id =  {
  fun_id : Charon.UllbcAst.FunDeclId.id;
  block_id : block_id;
  stm_id : statement_id;
}
end

module ProgramPointMap = Map.Make(struct
  type t = ProgramPoint.program_point_id
  let compare (pp1: ProgramPoint.program_point_id) (pp2: ProgramPoint.program_point_id) = 
    let diff_fun_id = Charon.UllbcAst.FunDeclId.compare_id pp1.fun_id pp2.fun_id in
    if diff_fun_id != 0 then diff_fun_id else compare (pp1.block_id, pp1.stm_id) (pp2.block_id, pp2.stm_id)
end
)


type value = 
  | Cons of Charon.Values.big_int
  | Borrow of place * borrow_kind
  | Bottom

type expression =
  | Val of value
  | Copy of place
  | Move of place
  | Borrow of place * borrow_kind

let print_expression ?(channel = stdout) (crate: Charon.UllbcAst.crate) (expr: expression) =  
  match expr with
  | Val(Cons cst) -> Printf.fprintf channel "Value : %s" (Charon.PrintValues.big_int_to_string cst)
  | Val(Borrow(place, b_kind)) -> let place = Charon.PrintExpressions.place_to_string (Charon.PrintUtils.of_crate crate) place in
    (match b_kind with 
    | BShared -> Printf.fprintf channel "Value : Shared borrow at %s" place
    | BMut -> Printf.fprintf channel "Value : Mutable borrow at %s" place
    | BTwoPhaseMut -> Printf.fprintf channel "Value : Two phase mutable borrow at %s" place
    | BShallow -> Printf.fprintf channel "Value : Shallow borrow at %s" place
    | BUniqueImmutable -> Printf.fprintf channel "Value : Unique immutable borrow at %s" place)
  | Copy place -> let place = Charon.PrintExpressions.place_to_string (Charon.PrintUtils.of_crate crate) place in
                  Printf.fprintf channel "Copy of %s" place
  | Move place -> let place = Charon.PrintExpressions.place_to_string (Charon.PrintUtils.of_crate crate) place in
                  Printf.fprintf channel "Move of %s" place
  | _ -> ()

let to_expr (rvalue: rvalue): expression =
  match rvalue with
  | Use _ -> failwith "Unsupported operation"
  | RvRef _ -> failwith "Unsupported operation"
  | RawPtr _ -> failwith "Unsupported operation"
  | BinaryOp _ -> failwith "Unsupported operation"
  | UnaryOp _ -> failwith "Unsupported operation"
  | NullaryOp _ -> failwith "Unsupported operation"
  | Discriminant _ -> failwith "Unsupported operation"
  | Aggregate _ -> failwith "Unsupported operation"
  | Global _ -> failwith "Unsupported operation"
  | GlobalRef _ -> failwith "Unsupported operation"
  | Len _ -> failwith "Unsupported operation"
;;

let interpreter crate = 
  (* For each block of a crate, interprets its content and prints the memory at each program point*)
  let rec place_helper (sigma: value VarIdMap.t) place counter : var_id =
    match place, counter with
    | PlaceBase var_id, 0 -> var_id
    | PlaceBase (var_id: var_id), _ -> assert (counter > 0); 
      (match VarIdMap.find var_id sigma with
      | Cons _ -> failwith "Illegal dereference"
      | Borrow(place, _b_kind) -> (match place.kind with                                (* TODO : pay attention to the borrow_kind*)
        | PlaceBase _ -> place_helper sigma place.kind (counter - 1)
        | PlaceProjection(_place, Deref) -> failwith "Double borrow without local deteced"
        | PlaceProjection(_place, Field _) -> failwith "Fields aren't supported yet")
      | Bottom -> failwith "Using a dropped value")
    | PlaceProjection(place, Deref), _ -> place_helper sigma place.kind (counter + 1)
    | PlaceProjection(_place, Field(_)), _ -> failwith "Fields aren't supported yet"
  in

  let interpret_expr (sigma: value VarIdMap.t) (expr: expression) =
    match expr with
    | Val(Cons cst) -> sigma, (Cons cst)
    | Val(Borrow(place, _b_kind)) -> 
      let p_string = Charon.PrintExpressions.place_to_string (Charon.PrintUtils.of_crate crate) place in
      Printf.eprintf "Expected a constant, found %s" p_string; raise Plonk 
    | Val(Bottom) -> raise Plonk
    | Copy(place) -> let var_id = place_helper sigma place.kind 0 in sigma, VarIdMap.find var_id sigma
    | Move(place)  -> 
      (match place.kind with
      | PlaceBase var_id -> let temp = VarIdMap.find var_id sigma in VarIdMap.add var_id Bottom sigma, temp
      | _ -> failwith "Complicated move that involves ownership")
    | Borrow(place, b_kind) -> let var_id = place_helper sigma place.kind 0 in
      sigma, Borrow({kind = PlaceBase(var_id); ty = place.ty}, b_kind)
  in

  let interpret_stm (sigma: value VarIdMap.t) (stm: Charon.UllbcAst.statement) =
  match stm.content with
  | Assign(place, rvalue) -> let var_id = place_helper sigma place.kind 0 in 
    let sigma', value = interpret_expr sigma (to_expr rvalue) in VarIdMap.add var_id value sigma'

  | StorageDead _ -> Printf.eprintf "TODO : Storage dead"; sigma
  | Drop _ -> Printf.eprintf "TODO : Drop"; sigma

  | FakeRead _ -> sigma
  | Nop -> sigma

  | _ -> failwith "Non exhaustive pattern"
  in
  
  Charon.Types.FunDeclId.Map.iter (fun _ (fun_decl: Charon.UllbcAst.blocks Charon.UllbcAst.gfun_decl) -> 
    match fun_decl.body with | Some(bdy: Charon.UllbcAst.blocks Charon.UllbcAst.gexpr_body) -> ()
                             | None -> ()
  ) crate.fun_decls
;;
