open ASD
open Token

(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

(* p (sep p)* *)
let rec list1 p sep = parser
  | [< x = p; l = list1_aux p sep >] -> x :: l
and list1_aux p sep = parser
  | [< _ = sep; l = list1 p sep >] -> l
  | [<>] -> []

(* (p (sep p)* )? *)
let list0 p sep = parser
  | [< l = list1 p sep >] -> l
  | [<>] -> []


(* TODO : change when you extend the language *)
let rec program =  parser
  | [< e =  many block ; _ = Stream.empty ?? "unexpected input at the end" >] -> Prog(e)

and block = parser 
  | [< 'LB ; declaration = many declar ; instr = many instruction;'RB >] -> Unit(declaration, instr)

and declar = parser  
  |[< 'INT_KW ; content =  list1 (variable) (comma) >] -> Declaration(content)

and variable = parser 
  |[< 'IDENT content  >] -> Var(content, None)
  |[< 'TAB (id,size) >] -> Tab(id, size , [])

and item = parser 
  |[< 'TEXT s >] -> Str(s) 
  |[< content = expression >] -> Expr(content)

and instruction = parser 
  | [< var = variable ; 'ASSIGN ; expr = expression >] -> Affect(var, expr) 
  | [<'PRINT_KW ; items =  list1 item comma >] -> Print(items)
  | [< 'READ_KW ; var = list1 variable comma >] -> Read(var)  
  | [< 'IF_KW ; expr = expression ; 'THEN_KW ; instr = instruction ; _ = (opt else_parser) ;  instr2 = (opt instruction)  ; 'FI_KW >] -> If(expr, instr, instr2)
  | [< 'WHILE_KW ; expr = expression ; 'DO_KW ;  instr = instruction ; 'OD_KW >] -> While(expr, instr) 
  | [< content = block>] -> Block(content)



and expression = parser
  | [< 'LP ; e = expression ; 'RP >] -> ParentheseExpression e 
  | [< e1 = factor; e = expression_aux e1 >] -> e

and expression_aux e1 = parser
  | [< 'PLUS;  e2 = factor; e = expression_aux (AddExpression (e1, e2)) >] -> e
  | [< 'MINUS;  e2 = factor; e = expression_aux (MinusExpression (e1, e2)) >] -> e
  | [< 'MUL;  e2 = factor; e = expression_aux (MulExpression (e1, e2)) >] -> e
  | [< 'DIV;  e2 = factor; e = expression_aux (DivExpression (e1, e2)) >] -> e
  | [<>] -> e1
  (* TODO : that's all? *)

and factor = parser
  | [< e1 = primary; e = factor_aux e1 >] -> e
  | [< e = expression >] -> e

and factor_aux e1 = parser
  | [<>] -> e1
  (* TODO : that's all? *)

and primary = parser
  | [< 'INTEGER x >] -> IntegerExpression x
  | [< 'IDENT x>] -> VarExpression(x)
  | [<>] -> failwith("Bad Parsing 2")
  (* TODO : that's all? *)

and comma = parser
  | [< 'COM >] -> ()

and else_parser = parser 
  | [< 'ELSE_KW >] -> () 

