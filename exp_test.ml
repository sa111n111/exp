type expr = 
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Var of string
  | Let of string;;

let rec form exp = 
  match exp with 
  | Add(lval, rval) -> "(left, right) -> (" ^ form lval ^ "+" ^ form rval ^ ")"
  | Sub(lval, rval) -> "(left, right) -> (" ^ form lval ^ "-" ^ form rval ^ ")"
  | Mult(lval, rval) -> "(left, right) -> (" ^ form lval ^ "*" ^ form rval ^ ")"
  | Div(lval, rval) -> "(left, right) -> (" ^ form lval ^ "/" ^ form rval ^ ")" 
  | Var v -> v
  | Let l -> l;;
                       
