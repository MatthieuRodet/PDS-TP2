(* TODO : extend when you extend the language *)

type ident = string

and variable = Var of ident * int option | Tab of ident * int * int option list 

and expression =
  | AddExpression of expression * expression
  | MinusExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression 
  | ParentheseExpression of expression
  | IntegerExpression of int
  | VarExpression of ident


and typ =
  | Type_Int

and item = Expr of expression | Str of string 

and declar = Declaration of variable list

and instruction = Affect of variable * expression 
              | Print of item list 
              | Read of variable list
              | If of expression * instruction * instruction option
              | While of expression * instruction 
              | Block of block


and block = Unit of declar list  * instruction list

and program = Prog of block list 


