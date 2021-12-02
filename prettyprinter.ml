open ASD

(* main function. return only a string *)
let rec prettyprint ast = match ast with 
  | Prog([]) -> ""
  | Prog(a::q) ->  prettyprint_func a  ^ "\n" ^  prettyprint (Prog(q)) 

and prettyprint_func ast = match ast with 
  |Proto(t, id, v) -> "PROTO " ^ prettyprint_type t ^ " " ^ id ^ " ("  ^ prettyprint_args(v) ^")\n"
  |Func(t, id, v , instruc) -> "FUNC " ^ prettyprint_type t ^ " " ^ id ^ " (" ^ prettyprint_args(v) ^ ")\n" ^ prettyprint_instr instruc 1 

and prettyprint_type t = match t with 
  |T_Int -> "INT"
  |T_Void -> "VOID"

and prettyprint_args v = match v with 
  |[] -> ""
  |[a] -> prettyprint_params a 
  |a::q -> prettyprint_params a ^ ", " ^ prettyprint_args q 

and prettyprint_params a = match a with 
  |Var_params(id) -> id 
  |Tab_params(id) -> id ^ "[" ^ "]"

and prettyprint_block ast n = match ast with 
  | Unit([],[]) -> ""
  | Unit([],[a]) -> prettyprint_instr a (n+1)
  | Unit([], a::q) -> prettyprint_instr a (n+1) ^ prettyprint_block (Unit([], q)) n
  | Unit([a],l) -> prettyprint_declar a (n+1) ^ "\n" ^ prettyprint_block (Unit([], l)) n
  | Unit(a::q, l) -> prettyprint_declar a (n+1) ^ prettyprint_block (Unit(q,l)) n

and prettyprint_instr ast n = match ast with 
  | Affect(var, expr) -> tabs n ^ prettyprint_variable var ^ " := " ^ prettyprint_expression expr ^ "\n"
  | Print(l) -> tabs n ^ "PRINT " ^ prettyprint_print l ^ "\n"
  | Read(l) -> tabs n ^ "READ " ^ prettyprint_read l ^ "\n"
  | Thread(id1, id2, q) -> tabs n ^ "THREAD " ^ id1 ^ ", " ^ id2 ^ ", " ^ prettyprint_many_expressions q ^ "\n"
  | Join(id1, v) -> tabs n ^ "JOIN " ^ id1 ^ ", " ^ prettyprint_variable v ^ "\n" 
  | Map(doc, x, f, params) -> tabs n ^ "MAP " ^ prettyprint_variable doc ^ ", " ^ string_of_int(x) ^ ", " ^ f ^ ", " ^ prettyprint_many_expressions params^ "\n"
  | Reduce(v) -> tabs n ^ "REDUCE " ^ prettyprint_variable v ^ "\n"
  | If(expr, i1, i2) -> prettyprint_if expr i1 i2 n
  | While(exp, instr) -> tabs n ^ "WHILE " ^ prettyprint_expression exp ^ " DO\n" ^ prettyprint_instr instr (n+1) ^ tabs n ^ "DONE\n"
  | Ret(exp) -> tabs n ^ "RETURN " ^ prettyprint_expression exp ^ "\n"
  | Block(b) -> tabs n ^ "{\n" ^ prettyprint_block b n ^ tabs n ^ "}\n"
  | Call(name, l) -> tabs n ^ name ^ "(" ^ prettyprint_many_expressions l ^")\n"

and prettyprint_if expr i1 i2 n = match i2 with 
  | Some(x) -> tabs n ^ "IF " ^ prettyprint_expression expr ^ " THEN\n" ^ prettyprint_instr i1 (n+1) ^ tabs n ^ "ELSE\n" ^ prettyprint_instr x (n+1) ^ tabs n ^ "FI\n" 
  | None -> tabs n ^ "IF " ^ prettyprint_expression expr ^ " THEN\n" ^ prettyprint_instr i1 (n+1) ^ tabs n ^ "FI\n" 
  
and prettyprint_print l = match l with 
  | [] -> ""
  | [a] -> prettyprint_item a 
  | a::q -> prettyprint_item a ^ ", " ^ prettyprint_print q 

and prettyprint_item a = match a with 
  | Expr(expr) -> prettyprint_expression expr
  | Str(s) -> "\"" ^ s ^ "\"" 
and prettyprint_read a = match a with 
  | [] -> ""
  | [a] -> prettyprint_variable a 
  | a::q -> prettyprint_variable a ^ ", " ^ prettyprint_read q 

and prettyprint_declar ast n = match ast with 
  | Declaration([]) -> ""
  | Declaration(l) -> tabs n ^ "INT " ^ prettyprint_many_decl_variables l ^ "\n"

and prettyprint_many_decl_variables  ast = match ast with 
  | [] -> ""
  | [a] -> prettyprint_decl_variable a
  | a::q -> prettyprint_decl_variable a ^ ", " ^ prettyprint_many_decl_variables (q)

and prettyprint_decl_variable ast = match ast with 
  | DVar(str) -> str
  | DTab(id, size) -> id ^ "[" ^ string_of_int size ^ "]"

and prettyprint_many_variables  ast = match ast with 
  | [] -> ""
  | [a] -> prettyprint_variable a
  | a::q -> prettyprint_variable a ^ ", " ^ prettyprint_many_variables (q)

and prettyprint_variable ast = match ast with 
  | Var(str) -> str
  | Tab(id, size) -> id ^ "[" ^ prettyprint_expression size ^ "]"

and prettyprint_many_expressions ast = match ast with 
  |[] -> ""
  |[a] -> prettyprint_expression a 
  |a::q -> prettyprint_expression a ^ ", " ^ prettyprint_many_expressions q

and prettyprint_expression ast = match ast with
| AddExpression ([]) -> ""
| AddExpression ([e]) -> prettyprint_prio1 e
| AddExpression (e1::e2) -> prettyprint_prio1 e1 ^ " + " ^ prettyprint_expression (AddExpression e2)
| MinusExpression ([]) -> ""
| MinusExpression ([e]) -> prettyprint_prio1 e
| MinusExpression (e1::e2) -> prettyprint_prio1 e1 ^ " + " ^ prettyprint_expression (MinusExpression e2)
| Unit1(e) -> prettyprint_prio1 e

and prettyprint_prio1 ast = match ast with
| MulExpression ([]) -> ""
| MulExpression ([e]) -> prettyprint_prio0 e
| MulExpression (e1::e2) -> prettyprint_prio0 e1 ^ " + " ^ prettyprint_prio1 (MulExpression e2)
| DivExpression ([]) -> ""
| DivExpression ([e]) -> prettyprint_prio0 e
| DivExpression (e1::e2) -> prettyprint_prio0 e1 ^ " + " ^ prettyprint_prio1 (DivExpression e2)
| Unit0(e) -> prettyprint_prio0 e

and prettyprint_prio0 ast = match ast with
  | ParentheseExpression (exp) -> "(" ^ (prettyprint_expression exp) ^ ")"
  | IntegerExpression i -> string_of_int i
  | VarExpression i -> i
  | CallFun(name, l) -> name ^ "(" ^ prettyprint_many_expressions l ^")"
  | TabExpression(id, e) -> id ^ "[" ^ prettyprint_expression e ^ "]" 


and tabs n = match n with
  | 0 -> ""
  | _ -> "    " ^ tabs (n-1)
(* TODO : extend when you extend the language *)