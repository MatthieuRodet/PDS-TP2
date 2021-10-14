open ASD

(* main function. return only a string *)
let rec prettyprint ast = match ast with 
  | Prog([]) -> ""
  | Prog(a::q) -> "{\n" ^ prettyprint_block a 0 ^ "}\n" ^  prettyprint (Prog(q)) 

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
  | If(expr, i1, i2) -> prettyprint_if expr i1 i2 n
  | While(exp, instr) -> tabs n ^ "While " ^ prettyprint_expression exp ^ " DO\n" ^ prettyprint_instr instr (n+1) ^ tabs n ^ "DONE\n"
  | Block(b) -> tabs n ^ "{\n" ^ prettyprint_block b n ^ tabs n ^ "}\n"

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
  | Declaration(l) -> tabs n ^ "INT " ^ prettyprint_many_variables l ^ "\n"

and prettyprint_many_variables  ast = match ast with 
  | [] -> ""
  | [a] -> prettyprint_variable a
  | a::q -> prettyprint_variable a ^ ", " ^ prettyprint_many_variables (q)

and prettyprint_variable ast = match ast with 
  | Var(str,i) -> str
  | Tab(id, size, tab) -> id ^ "[" ^ string_of_int size ^ "]"

and prettyprint_expression ast = match ast with
  | AddExpression (l, r) -> (prettyprint_expression l) ^ " + " ^ (prettyprint_expression r)
  | MinusExpression (l, r) -> (prettyprint_expression l) ^ " - " ^ (prettyprint_expression r)
  | MulExpression (l, r) -> (prettyprint_expression l) ^ " * " ^ (prettyprint_expression r)
  | DivExpression (l, r) -> (prettyprint_expression l) ^ " / " ^ (prettyprint_expression r)
  | ParentheseExpression (exp) -> "(" ^ (prettyprint_expression exp) ^ ")"
  | IntegerExpression i -> string_of_int i
  | VarExpression i -> i

and tabs n = match n with
  | 0 -> ""
  | _ -> "    " ^ tabs (n-1)
(* TODO : extend when you extend the language *)
