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
  | [< e =  many func ; _ = Stream.empty ?? "unexpected input at the end" >] -> Prog(e)

and func = parser 
  | [< 'PROTO_KW ; content = proto >] -> content
  | [< 'FUNC_KW ;  content = function_block >] -> content

and declar_function = parser 
   [< ret_type = parse_type ; name = parse_name ; 'LP ; variables = list0 (variable) (comma)  ; 'RP >] -> ret_type, name, variables

and parse_type = parser 
  |[< 'INT_KW >] -> T_Int
  |[< 'VOID_KW >] -> T_Void

and parse_name = parser 
  |[< 'IDENT content >] -> content

and proto = parser 
  | [< ret_type,  name , variables = declar_function >] -> Proto(ret_type, name, variables)

and function_block = parser
  | [< ret_type , name ,variables = declar_function ; instr = instruction >] -> Func(ret_type, name, variables, instr)

and block = parser 
  | [< 'LB ; declaration = many declar ; instr = many instruction;'RB >] -> Unit(declaration, instr)

and declar = parser  
  |[< 'INT_KW ; content =  list1 (variable) (comma) >] -> Declaration(content)

and variable = parser 
  |[< 'IDENT content  >] -> Var(content)
  |[< 'TAB (id,size) >] -> Tab(id, size)

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
  | [< e1 = parsePrio1 ; aux = expression_aux e1 >] -> aux

and expression_aux e1 = parser
  | [< 'PLUS;  e2 = list1 parsePrio1 plus >] -> AddExpression(e1::e2)
  | [< 'MINUS;  e2 = list1 parsePrio1 minus >] -> MinusExpression(e1::e2)
  | [< >] -> Unit1 e1

and parsePrio1 = parser
  | [< e1 = parsePrio0 ; aux = parsePrio1_aux e1 >] -> aux

and parsePrio1_aux e1 = parser
  | [< 'MUL;  e2 = list1 parsePrio0 mul >] -> MulExpression(e1::e2)
  | [< 'DIV;  e2 = list1 parsePrio0 div >] -> DivExpression(e1::e2)
  | [< >] -> Unit0 e1

and parsePrio0 = parser
  | [< 'LP ; e = expression ; 'RP >] -> ParentheseExpression e 
  | [< 'INTEGER x >] -> IntegerExpression x
  | [< 'IDENT x>] -> VarExpression x

and plus = parser
  | [< 'PLUS >] -> ()

and minus = parser
  | [< 'MINUS >] -> ()

and mul = parser
  | [< 'MUL >] -> ()

and div = parser
  | [< 'DIV >] -> ()

and comma = parser
  | [< 'COM >] -> ()

and else_parser = parser 
  | [< 'ELSE_KW >] -> ()