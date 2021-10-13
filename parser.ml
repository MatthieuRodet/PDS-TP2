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
let rec program = print_string("program_parser: ok \n"); parser
  | [< e =  many block ; _ = Stream.empty ?? "unexpected input at the end" >] -> print_string("program_parser: ok "); Prog(e)

and block = print_string("block_parser: ok \n "); parser 
  | [< declaration = many declar ; instr = many expression >] ->  print_string("block_parser : ok "); Unit(declaration, instr)

and declar = print_string("declar_parser: ok \n");parser  
  |[< 'INT_KW ; content = list1 (declar_variable) (comma) >] ->print_string("declar_parser : ok ");  Declaration(content)

and declar_variable = print_string("declar_var_parser: ok "); parser 
  |[< 'TEXT(content)  >] -> Var(content, None)
  |[<>] -> failwith("Bad parsing")
and expression = print_string("expression_parser: ok \n"); parser
  | [< 'LP ; e = expression ; 'RP >] -> ParentheseExpression e 
  | [< e1 = factor; e = expression_aux e1 >] -> e

and expression_aux e1 = print_string("expression_aux_parser: ok \n");parser
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
  | [<>] -> failwith("Bad Parsing 2")
  (* TODO : that's all? *)

and comma = parser
  | [< 'COM >] -> ()
