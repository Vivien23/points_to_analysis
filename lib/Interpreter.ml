
module VarIdMap = Map.Make(struct
  type t = Charon.Types.type_var_id
  let compare : t -> t -> int = Charon.Types.TypeVarId.compare_id 
end)


type value = 
  | Cons of Charon.Values.integer_type
  | Borrow of Charon.Expressions.place * Charon.Expressions.borrow_kind
;;

let interpret_stm (sigma: value VarIdMap.t) (stm: Charon.UllbcAst.statement) =
  ()
;;
