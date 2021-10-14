open ASD

(* main function. return only a string *)
let rec prettyprint ast = match ast with 
  |Prog([]) -> ""
  |Prog(a::q) -> "{\n" ^ prettyprint_block a ^ "}\n" ^  prettyprint (Prog(q)) 

and prettyprint_block ast = match ast with 
  |Unit([],[]) -> ""
  |Unit([],[a]) -> prettyprint_instr a ^ "\n"
  |Unit([], a::q) -> prettyprint_instr a ^ "\n" ^ prettyprint_block (Unit([], q)) 
  |Unit([a],l) -> prettyprint_declar a ^ "\n" ^ prettyprint_block (Unit([], l))
  |Unit(a::q, l) -> prettyprint_declar a ^ "\n" ^ prettyprint_block (Unit(q,l))

and prettyprint_instr ast = match ast with 
  | Affect(var, expr) -> prettyprint_variable var ^ " := " ^ prettyprint_expression expr
  | Print(l) -> "PRINT " ^ prettyprint_print l 
  | Read(l) -> "READ " ^ prettyprint_read l 
  | If(expr, i1, i2) -> prettyprint_if expr i1 i2 
   |While(exp, instr) -> "While " ^ prettyprint_expression exp ^ "{" ^ prettyprint_instr instr ^ "}"
  |Block(b) -> "{" ^ prettyprint_block b ^ "}"

and prettyprint_if expr i1 i2 = match i2 with 
        |Some(x) -> "If " ^ prettyprint_expression expr ^ "THEN " ^ prettyprint_instr i1 ^ "ELSE " ^ prettyprint_instr x ^ " FI" 
        |None -> "If " ^ prettyprint_expression expr ^ "THEN " ^ prettyprint_instr i1 ^ " Fi" 
  
and prettyprint_print l = match l with 
  |[] -> ""
  |[a] -> prettyprint_item a 
  |a::q -> prettyprint_item a ^ ", " ^ prettyprint_print q 

and prettyprint_item a = match a with 
  | Expr(expr) -> prettyprint_expression expr
  |Str(s) -> "\"" ^ s ^ "\"" 
and prettyprint_read a = match a with 
  |[] -> ""
  |[a] -> prettyprint_variable a 
  |a::q -> prettyprint_variable a ^ ", " ^ prettyprint_read q 

and prettyprint_declar ast = match ast with 
  |Declaration([]) -> ""
  |Declaration(l) -> "INT " ^ prettyprint_many_variables l

and prettyprint_many_variables  ast = match ast with 
  |[] -> ""
  |[a] -> prettyprint_variable a
  |a::q -> prettyprint_variable a ^ "," ^ prettyprint_many_variables (q)

and prettyprint_variable ast = match ast with 
  |Var(str,i) -> str
  |Tab(id, size, tab) -> id ^ "[" ^ string_of_int size ^ "]"

and prettyprint_expression ast =
  match ast with
  | AddExpression (l, r) -> (prettyprint_expression l) ^ " + " ^ (prettyprint_expression r)
  | MinusExpression (l, r) -> (prettyprint_expression l) ^ " - " ^ (prettyprint_expression r)
  | MulExpression (l, r) -> (prettyprint_expression l) ^ " * " ^ (prettyprint_expression r)
  | DivExpression (l, r) -> (prettyprint_expression l) ^ " / " ^ (prettyprint_expression r)
  | ParentheseExpression (exp) -> "(" ^ (prettyprint_expression exp) ^ ")"
  | IntegerExpression i -> string_of_int i
  | VarExpression i -> i

(* TODO : extend when you extend the language *)
