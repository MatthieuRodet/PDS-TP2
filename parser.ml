open ASD
open Token

(* p? *)
let opt p = parser
    | [< x = p >] -> Some x
    | [< >] -> None

(* p* *)
let rec many p = parser
    | [< x = p ; l = many p >] -> x :: l
    | [< >] -> []

(* p+ *)
let some p = parser
    | [< x = p ; l = many p >] -> x :: l

(* p (sep p)* *)
let rec list1 p sep = parser
    | [< x = p ; l = list1_aux p sep >] -> x :: l
and list1_aux p sep = parser
    | [< _ = sep ; l = list1 p sep >] -> l
    | [< >] -> []

(* (p (sep p)* )? *)
let list0 p sep = parser
    | [< l = list1 p sep >] -> l
    | [< >] -> []



(* MAIN PARSER *)
let rec program = parser
    | [< e = many func ; _ = Stream.empty ?? "unexpected input at the end" >] -> Prog(e)

(* FONCTIONS *)

and func = parser 
    | [< 'PROTO_KW ; content = proto >] -> content
    | [< 'FUNC_KW ; content = function_block >] -> content

and proto = parser 
    | [< ret_type, name, args = header_function >] -> Proto(ret_type, name, args)

and function_block = parser
    | [< ret_type, name, args = header_function ; instr = instruction >] -> Func(ret_type, name, args, instr)

and header_function = parser 
    | [< ret_type = parse_type ; 'IDENT name ; 'LP ; args = list0 parse_params comma ; 'RP >] -> ret_type, name, args

and parse_type = parser 
    | [< 'INT_KW >] -> T_Int
    | [< 'VOID_KW >] -> T_Void

and parse_params = parser 
    | [< 'IDENT id ; is_tab = parse_params_aux id >] -> is_tab

and parse_params_aux id = parser 
    | [< 'LC ; 'RC >] -> Tab_params id
    | [< >] -> Var_params id

(* INSTRUCTIONS *)

and block = parser 
    | [< 'LB ; declaration = many declar ; instr = many instruction ; 'RB >] -> Unit(declaration, instr)

and instruction = parser 
    | [< 'IDENT name ; content = call_affect_aux name >] -> content
    | [< 'PRINT_KW ; items = list1 item comma >] -> Print(items)
    | [< 'READ_KW ; var = list1 variable_parser comma >] -> Read(var)
    | [< 'IF_KW ; expr = expression ; 'THEN_KW ; instr = instruction ; _ = (opt else_parser) ; instr2 = (opt instruction) ; 'FI_KW >] -> If(expr, instr, instr2)
    | [< 'WHILE_KW ; expr = expression ; 'DO_KW ; instr = instruction ; 'OD_KW >] -> While(expr, instr) 
    | [< 'RETURN_KW ; expr = expression >] -> Ret(expr)
    | [< 'THREAD_KW ; 'IDENT id1 ; 'COM ; 'IDENT id2 ; args = parse_args_thread >] -> Thread(id1, id2, args)
    | [< 'JOIN_KW ; 'IDENT id1 ; res = parse_join_res >] -> Join(id1, res)
    | [< 'MAP_KW ; 'IDENT doc ; 'COM ; 'INTEGER frac ; 'COM ; 'INTEGER size ; 'COM ; 'IDENT f ; args = parse_args_map >] -> MapRed(doc, frac, size, f, args)
    | [< content = block>] -> Block(content)

and call_affect_aux id = parser 
    | [< 'LP; params = (list0 expression comma) ; 'RP >] -> Call(id, params)
    | [< vrbls = (variable_aux id) ; 'ASSIGN ; expr = expression >] -> Affect(vrbls, expr)

and parse_args_thread = parser
    | [< 'COM ; args = list1 expression comma >] -> args
    | [< >] -> []

and parse_join_res = parser
    | [< 'COM ; 'IDENT id >] -> Some(id)
    | [< >] -> None

and parse_args_map = parser
    | [< 'COM ; args = list1 expression comma >] -> args
    | [< >] -> [] 

(* EXPRESSIONS *)

and expression = parser
    | [< e1 = parsePrio1 ; aux = expression_aux e1 >] -> aux

and expression_aux e1 = parser
    | [< 'PLUS ; e2 = list1 parsePrio1 plus >] -> AddExpression(e1::e2)
    | [< 'MINUS ; e2 = list1 parsePrio1 minus >] -> MinusExpression(e1::e2)
    | [< >] -> Unit1 e1

and parsePrio1 = parser
    | [< e1 = parsePrio0 ; aux = parsePrio1_aux e1 >] -> aux

and parsePrio1_aux e1 = parser
    | [< 'MUL ; e2 = list1 parsePrio0 mul >] -> MulExpression(e1::e2)
    | [< 'DIV ; e2 = list1 parsePrio0 div >] -> DivExpression(e1::e2)
    | [< >] -> Unit0 e1

and parsePrio0 = parser
    | [< 'LP ; e = expression ; 'RP >] -> ParentheseExpression e 
    | [< 'INTEGER x >] -> IntegerExpression x
    | [< 'IDENT x ; is_fun = parse_call_fun x >] -> is_fun

and parse_call_fun id = parser 
    | [< 'LP ; params = (list0 expression comma) ; 'RP >] -> CallFun(id, params)
    | [< 'LC ; expr = expression ; 'RC >] -> TabExpression(id, expr)
    | [< >] -> VarExpression(id)

(* VARIABLES *)

and declar = parser
    | [< 'INT_KW ; content = list1 declar_var comma >] -> Declaration content
    | [< 'TID_KW ; content = list1 declar_var comma >] -> DeclarationTid content

and declar_var = parser 
    | [< 'IDENT id ; vrbls = declar_var_aux id >] -> vrbls
    
and declar_var_aux id = parser
    | [< 'LC ; 'INTEGER x ; 'RC >] -> DTab(id, x)
    | [< >] -> DVar(id)

and variable_parser = parser 
    | [< 'IDENT id ; vrbls = (variable_aux id)>] -> vrbls

and variable_aux id = parser 
    | [< 'LC ; size = expression; 'RC >] -> Tab(id, size)
    | [< >] -> Var(id)

and item = parser 
    | [< 'TEXT s >] -> Str(s) 
    | [< content = expression >] -> Expr(content)

(* SEPARATORS *)

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