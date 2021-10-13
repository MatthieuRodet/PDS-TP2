open ASD

(* main function. return only a string *)
let rec prettyprint ast = match ast with 
  |Prog([]) -> ""
  |Prog(a::q) -> prettyprint_block a ^  prettyprint (Prog(q)) 

and prettyprint_block ast = match ast with 
  |Unit([],[]) -> ""
  |Unit([], [a]) -> prettyprint_expression a 
  |Unit([], a::q) -> prettyprint_expression a ^ prettyprint_block (Unit([],q))
  |Unit([a], q) -> prettyprint_declar a ^ prettyprint_block (Unit([], q))
  |Unit(a::q , e) -> prettyprint_declar a ^ prettyprint_block (Unit(q,e)) 

and prettyprint_declar ast = match ast with 
  |Declaration([]) -> ""
  |Declaration(l) -> "INT " ^ prettyprint_many_variables l

and prettyprint_many_variables  ast = match ast with 
  |[] -> ""
  |a::q -> prettyprint_variable a ^ "," ^ prettyprint_many_variables (q)

and prettyprint_variable ast = match ast with 
  |Var(str,i) -> str

and prettyprint_expression ast =
  match ast with
  | AddExpression (l, r) -> (prettyprint_expression l) ^ " + " ^ (prettyprint_expression r)
  | MinusExpression (l, r) -> (prettyprint_expression l) ^ " - " ^ (prettyprint_expression r)
  | MulExpression (l, r) -> (prettyprint_expression l) ^ " * " ^ (prettyprint_expression r)
  | DivExpression (l, r) -> (prettyprint_expression l) ^ " / " ^ (prettyprint_expression r)
  | ParentheseExpression (exp) -> "(" ^ (prettyprint_expression exp) ^ ")"
  | IntegerExpression i -> string_of_int i
  | VarExpression i -> failwith ("TODO")

(* TODO : extend when you extend the language *)
