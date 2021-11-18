(* TODO : extend when you extend the language *)

type ident = string

and variable =
  | Var of ident
  | Tab of ident * int

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

and typ =
  | Type_Int

and item =
  | Expr of expression
  | Str of string 

and declar =
  | Declaration of variable list

and instruction = 
  | Affect of variable * expression 
  | Print of item list 
  | Read of variable list
  | If of expression * instruction * instruction option
  | While of expression * instruction 
  | Block of block

and block =
  | Unit of declar list * instruction list

and ret_type = 
  |T_Int 
  |T_Void

and func = 
  |Proto of ret_type*ident*(variable list) 
  |Func of ret_type*ident*(variable list)*instruction

and program = 
  |Prog of func list