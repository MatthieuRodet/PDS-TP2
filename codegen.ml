open ASD
open Llvm
open Utils
open SymbolTable

       
(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir = 
  let ir, v = ir_of_prog prog in
  (* adds the return instruction *)
  let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in
  (* We create the function main *)
  let ir = llvm_define_main ir in
  ir

and ir_of_prog : program -> llvm_ir * llvm_value = function 
  |Prog([]) -> failwith "todo"
  |Prog([a]) -> failwith "todo" 
  |Prog(a::q) -> failwith "todo" 

and ir_of_block = function 
  |([], []) -> failwith "todo" 
  |([a], l) -> failwith "todo"
  | (a::q , l ) -> failwith "todo"
  |([], [a]) -> failwith "todo"
  |([], a::q) -> failwith "todo" 

and ir_of_instruction : instruction -> llvm_ir = function 
  |Affect(Var(v, _),e) -> let ir, out = ir_of_expression e in
                  ir @: (llvm_affect_var out v)
  |Affect(Tab(v, _, _),e) -> failwith "todo"
  |Print([]) -> empty_ir 
  |Print(a::q) -> failwith "todo"
  |Read([]) -> empty_ir
  |Read(a::q) -> failwith "todo" 
  |If(e,i,io) -> failwith "todo" 
  |While(e,i) -> failwith "todo"
  |Block(b) -> failwith "todo"

and ir_of_declaration = function 
  |Declaration([]) -> Empty
  |Declaration([a]) -> Atom(aux_declaration a)
  |Declaration(a::q) -> Concat(ir_of_declaration (Declaration [a]), ir_of_declaration (Declaration q))

and aux_declaration dec = match dec with 
  |Var(id, _) -> let ir = llvm_declar_var_int ~res_var:id ~res_type:LLVM_type_i32 in ir 
  |Tab(id, size, _) -> let ir = llvm_declar_var_tab ~res_tab:id ~res_size:(LLVM_i32 size) ~res_type:LLVM_type_i32 in ir

and ir_of_variable = function 
  |Var(id, v) -> failwith "todo"
  |Tab(id, i , l) -> failwith "todo"

and ir_of_item i t = match i with 
  |Expr(e) -> failwith "todo"
  |Str(s) -> failwith "todo"


(* translation from VSL+ types to LLVM types *)
and llvm_type_of_asd_typ : typ -> llvm_type = function
  | Type_Int -> LLVM_type_i32

(* all expressions have type LLVM_type_i32 *)
(* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression : expression -> llvm_ir * llvm_value = function
  | IntegerExpression i ->
     empty_ir, LLVM_i32 i
  | AddExpression (e1,e2) ->
     let ir1, v1 = ir_of_expression e1 in
     let ir2, v2 = ir_of_expression e2 in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | MulExpression (e1,e2) -> 
      let ir1, v1 = ir_of_expression e1 in
      let ir2, v2 = ir_of_expression e2 in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  | MinusExpression (e1,e2) -> 
      let ir1, v1 = ir_of_expression e1 in
      let ir2, v2 = ir_of_expression e2 in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  | DivExpression (e1,e2) -> 
      let ir1, v1 = ir_of_expression e1 in
      let ir2, v2 = ir_of_expression e2 in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_udiv ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  |ParentheseExpression e -> ir_of_expression e
  |VarExpression e -> failwith "todo"
(* TODO: complete with new cases and functions when you extend your language *)
