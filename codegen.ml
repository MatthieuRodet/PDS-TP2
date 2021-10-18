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
  |Affect(v,e) -> failwith "todo" 
  |Print([]) -> failwith "todo" 
  |Print(a::q) -> failwith "todo"
  |Read([]) -> failwith "todo"
  |Read(a::q) -> failwith "todo" 
  |If(e,i,io) -> failwith "todo" 
  |While(e,i) -> failwith "todo"
  |Block(b) -> failwith "todo"

and ir_of_declaration = function 
  |Declaration([]) -> failwith "todo"
  |Declaration([a]) -> failwith "todo"
  |Declaration(a::q) -> failwith "todo"

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
and ir_of_expression : expression * variable_table -> llvm_ir * llvm_value = function
  | (IntegerExpression i, t) ->
     empty_ir, LLVM_i32 i
  | (AddExpression (e1,e2), t ) ->
     let ir1, v1 = ir_of_expression (e1,t) in
     let ir2, v2 = ir_of_expression (e2,t) in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | (MulExpression (e1,e2),t) -> 
      let ir1, v1 = ir_of_expression (e1,t) in
      let ir2, v2 = ir_of_expression (e2,t) in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  | (MinusExpression (e1,e2),t) -> 
      let ir1, v1 = ir_of_expression (e1,t) in
      let ir2, v2 = ir_of_expression (e2,t) in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  | (DivExpression (e1,e2),t) -> 
      let ir1, v1 = ir_of_expression (e1,t) in
      let ir2, v2 = ir_of_expression (e2,t) in 
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_udiv ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
  |(ParentheseExpression (e),t) -> ir_of_expression (e,t) 
  |(VarExpression(e),t) -> let var = get_variable_value t e in match var with 
        |None -> failwith "Undeclared variable"
        |Some(x) -> ir_of_expression ((IntegerExpression x),t )



(* TODO: complete with new cases and functions when you extend your language *)
