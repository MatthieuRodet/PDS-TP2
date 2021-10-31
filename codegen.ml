open ASD
open Llvm
open Utils
open SymbolTable



(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir = 
  (*let ir, v = ir_of_prog prog in*)
  let ir = ir_of_prog prog in 
  let v = LLVM_i32(0) in 
  (* adds the return instruction *)
  let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in
  (* We create the function main *)
  let ir = llvm_define_main ir in
  ir

and ir_of_prog (prog :  program) : llvm_ir = match prog with 
  |Prog([]) ->  empty_ir
  |Prog([a]) -> ir_of_block a
  |Prog(a::q) -> ir_of_block a @@ ir_of_prog (Prog q)

and ir_of_block (b : block) : llvm_ir = match b with 
  |Unit(declar, instr) -> ir_of_declaration declar @@ ir_of_instructions instr

and ir_of_instructions ( l : instruction list) : llvm_ir = match l with 
  |[] -> empty_ir
  |[a] -> ir_of_instruction a
  |a::q -> ir_of_instruction a @@ ir_of_instructions q

and ir_of_instruction  (instr : instruction) : llvm_ir = match instr with 
  |Affect(Var(v, _),e) -> let ir, out = ir_of_expression e in
                                 ir @: (llvm_affect_var out v)
  |Affect(Tab(v, _, _),e) -> failwith "todo affect tab"
  |Print([]) -> empty_ir
  |Print(items) -> ir_of_print items
  |Read([]) -> empty_ir
  |Read(a::q) -> ir_of_read a @@ ir_of_instruction (Read q)
  |If(e,i,io) -> ir_of_if e i io
  |While(e,i) -> ir_of_while e i
  |Block(b) -> ir_of_block b

and ir_of_print (a : item list) : llvm_ir = 
    let rec aux_print (a : item list) (ir : llvm_ir) (to_print : string) (args : llvm_value list): llvm_ir * string * llvm_value list =
      match a with
      | [] -> ir, to_print, args
      | (Str s)::q -> aux_print q ir (to_print ^ s) args
      | (Expr e)::q -> match e with
                | IntegerExpression(i) -> aux_print q ir (to_print ^ "%d") (args@[LLVM_i32 i])
                | _ -> let ir2, out = ir_of_expression e in
                       aux_print q (ir@@ir2) (to_print ^ "%d") (args@[out])
    in let ir, to_print, args = aux_print a empty_ir "" []
    in let x = newglob(".fmt")
    in let to_print_final, len = string_transform to_print
    in (ir @: llvm_print x len args) @^ llvm_str x len to_print_final

and ir_of_read (a : variable) : llvm_ir = match a with
    | Var(ident, _) -> (empty_ir @: llvm_read ident)
    | Tab(ident, _, _) -> failwith "todo : is 'READ tab[i]' valid ?"

and ir_of_if e i io : llvm_ir = match io with
    | None -> let ir_e, out = ir_of_expression e in
                 let out_cond = newtmp() in
                 let jump_if = newlab("Then") in
                 let jump_endif = newlab("Endif") in
                 let ir = ir_of_instruction i in
                 (((ir_e @: llvm_cmp out_cond out) @: llvm_if (LLVM_var out_cond) jump_if jump_endif) 
                 @: llvm_label jump_if)
                 @@ ((ir @: llvm_jump jump_endif)
                 @: llvm_label jump_endif)
    | Some(instr) -> let ir_e, out = ir_of_expression e in
                 let out_cond = newtmp() in
                 let jump_if = newlab("Then") in
                 let jump_else = newlab("Else") in
                 let jump_endif = newlab("Endif") in
                 let ir_then = ir_of_instruction i in
                 let ir_else = ir_of_instruction instr in
                 (((ir_e @: llvm_cmp out_cond out) @: llvm_if (LLVM_var out_cond) jump_if jump_else) 
                          @: llvm_label jump_if)
                          @@ ((ir_then @: llvm_jump jump_endif) 
                          @: llvm_label jump_else)
                          @@ ((ir_else @: llvm_jump jump_endif)
                          @: llvm_label jump_endif)

and ir_of_while (cond : expression) (bloc : instruction) : llvm_ir = 
    let ir_cond, out_cond = ir_of_expression cond in
    let cond = newtmp() in
    let ir_bloc = ir_of_instruction bloc in
    let lab_while = newlab "While" in
    let lab_bloc = newlab "Bloc" in
    let lab_end = newlab "EndWhile" in
    ((((((((empty_ir @: llvm_jump lab_while)
    @: (llvm_label lab_while))
    @@ (ir_cond @: (llvm_cmp cond out_cond)))
    @: (llvm_if (LLVM_var cond) lab_bloc lab_end))
    @: (llvm_label lab_bloc))
    @@ (ir_bloc))
    @: (llvm_jump lab_while))
    @: (llvm_label lab_end))

and ir_of_declaration (l : declar list ) : llvm_ir = match l with 
  |[] -> empty_ir 
  |[a] -> {header = Empty ; body = (aux0_declaration a) }
  |a::q ->  {header = Empty ; body = aux0_declaration a } @@ ir_of_declaration q  

and aux0_declaration (dec : declar ) : llvm_instr_seq = match dec with 
  |Declaration([]) -> Empty 
  |Declaration([a]) -> Atom(aux_declaration a)
  |Declaration(a::q) -> Concat(Atom(aux_declaration a), aux0_declaration (Declaration(q)))

and aux_declaration (var : variable ) : llvm_instr = match var with 
  |Var(id, _) -> let ir = llvm_declar_var_int ~res_var:id ~res_type:LLVM_type_i32 in ir 
  |Tab(id, size, _) -> let ir = llvm_declar_var_tab ~res_tab:id ~res_size:(LLVM_i32 size) ~res_type:LLVM_type_i32 in ir

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
  |VarExpression e -> 
      let out = newtmp() in
      empty_ir @: (llvm_var_expr out e), LLVM_var out
