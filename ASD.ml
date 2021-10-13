(* TODO : extend when you extend the language *)

type ident = string

type variable = Var of string * int option

type expression =
  | AddExpression of expression * expression
  | MinusExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression 
  | ParentheseExpression of expression
  | IntegerExpression of int
  | VarExpression of variable

type typ =
  | Type_Int

type declar = Declaration of variable list

type block = Unit of declar list  * expression list

type program = Prog of block list 


