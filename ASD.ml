(* TODO : extend when you extend the language *)

type ident = string


and variable =
  | Var of ident
  | Tab of ident * expression 
and decl_variable =
    | DVar of ident
    | DTab of ident * int 

and params = 
  | Var_params of ident
  | Tab_params of ident

and expression =
  | AddExpression of expPrio1 list
  | MinusExpression of expPrio1 list
  | Unit1 of expPrio1

and expPrio1 =
  | MulExpression of expPrio0 list
  | DivExpression of expPrio0 list
  | Unit0 of expPrio0

and expPrio0 =
  | ParentheseExpression of expression
  | IntegerExpression of int
  | VarExpression of ident
  | CallFun of ident* (expression list)
  | TabExpression of ident*expression 


and typ =
  | Type_Int
  | Type_Tab of int
  | Type_Tab_Ptr

and item =
  | Expr of expression
  | Str of string 

and declar =
  | Declaration of decl_variable list


and instruction = 
  | Affect of variable * expression 
  | Print of item list 
  | Read of variable list
  | Thread of ident * ident
  | Join of ident
  | If of expression * instruction * instruction option
  | While of expression * instruction 
  | Block of block
  | Ret of expression 
  | Call of ident* (expression list) 
  | MapRed of variable * int * ident * (expression list)

and block =
  | Unit of declar list * instruction list

and ret_type = 
  |T_Int 
  |T_Void


and func = 
  |Proto of ret_type*ident*(params list) 
  |Func of ret_type*ident*(params list)*instruction

and program = 
  |Prog of func list