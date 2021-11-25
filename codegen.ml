open ASD
open Llvm
open Utils
open SymbolTable



(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir = 
  (*let ir, v = ir_of_prog prog in*)
  let ir, sym_tab = ir_of_prog prog [] in 
  (*let v = LLVM_i32(0) in*) 
  (* adds the return instruction *)
  (*let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in*)
  (* We create the function main *)
  (*let ir = llvm_define_main ir in*)
  ir

and ir_of_prog (prog :  program) (sym_tab : symbol_table): llvm_ir * symbol_table = match prog with 
  |Prog([]) ->  empty_ir, []
  |Prog(a::q) -> let ir, decl = ir_of_fun a sym_tab in let ir2, decl2 = ir_of_prog (Prog q) ((FunctionSymbol decl)::sym_tab) in ir @@ ir2, (FunctionSymbol decl)::decl2

and ir_of_fun (f : func) (sym_tab : symbol_table) : llvm_ir * function_symbol = match f with
  |Proto(ret, id, args) -> empty_ir, {return_type=ret; identifier=newfun id; arguments=args; state=Declared}
  |Func(ret, id, args, body) -> let fun_head = llvm_fun_header (ret_type_conv ret) id (params_to_args args) in
          let arg_ir, arg_sym_tab = sym_tab_of_args args in
          match ret with
          | T_Void -> 
          (empty_ir @: fun_head) @@ arg_ir @@ ir_of_instruction body (arg_sym_tab @ sym_tab) @: "ret void\n}\n",
          {return_type=ret; identifier=id; arguments=args; state=Defined}
          | T_Int ->
          (empty_ir @: fun_head) @@ arg_ir @@ ir_of_instruction body (arg_sym_tab @ sym_tab) @: "}\n",
          {return_type=ret; identifier=id; arguments=args; state=Defined}

and sym_tab_of_args (args : params list) : llvm_ir * symbol_table =
  match args with
  | [] -> empty_ir, []
  | (Var_params id)::tl -> let uniqid = newuniqid id in
                           let ir, st = sym_tab_of_args tl in 
                           (ir @: llvm_declar_var_int uniqid LLVM_type_i32) @: llvm_affect_var (LLVM_var id) uniqid, VariableSymbol(Type_Int, id, uniqid)::st
  | (Tab_params id)::tl -> failwith("TODO : tabs in args") (*VariableSymbol(Type_Tab, id, id)::(sym_tab_of_list tl)*)

and ir_of_main (body : instruction) (sym_tab : symbol_table) : llvm_ir =
  let ir = ir_of_instruction body sym_tab in
  llvm_define_main ir
and ret_type_conv ret_type = match ret_type with
  | T_Int -> LLVM_type_i32
  | T_Void -> LLVM_type_void
and params_to_args (params : params list) =
  match params with
  | [] -> []
  | (Var_params id)::tl -> (LLVM_type_i32, id)::params_to_args tl
  | (Tab_params id)::tl -> (LLVM_type_i32_ptr, id)::params_to_args tl
and ir_of_instructions ( l : instruction list) (sym_tab : symbol_table) : llvm_ir = match l with 
  |[] -> empty_ir
  |[a] -> ir_of_instruction a sym_tab
  |a::q -> ir_of_instruction a sym_tab @@ ir_of_instructions q sym_tab

and ir_of_instruction  (instr : instruction) (sym_tab : symbol_table) : llvm_ir = match instr with 
  |Affect(Var(v),e) -> let ir, out = ir_of_expression e sym_tab in
                                 ir @: ir_of_affect_var v out sym_tab
  |Affect(Tab(v, _),e) -> failwith "todo affect tab"
  |Print([]) -> empty_ir
  |Print(items) -> ir_of_print items sym_tab
  |Read([]) -> empty_ir
  |Read(a::q) -> ir_of_read a sym_tab @@ ir_of_instruction (Read q) sym_tab
  |If(e,i,io) -> ir_of_if e i io sym_tab
  |While(e,i) -> ir_of_while e i sym_tab
  |Block(b) -> ir_of_block b sym_tab
  |Ret(e) -> let ir, ret_val = ir_of_expression e sym_tab in ir @: llvm_ret ret_val
  |Call(id, args) -> ir_of_call id args sym_tab

and ir_of_affect_var (var : ident) (value : llvm_value) (sym_tab : symbol_table): llvm_instr =
  match uniq_id_of_symbol_table sym_tab var with
  | None -> failwith ("Error : unknown var symbol " ^ var)
  | Some(id) -> llvm_affect_var value id

and ir_of_print (a : item list) (sym_tab : symbol_table) : llvm_ir = 
    let rec aux_print (a : item list) (ir : llvm_ir) (to_print : string) (args : llvm_value list): llvm_ir * string * llvm_value list =
      match a with
      | [] -> ir, to_print, args
      | (Str s)::q -> aux_print q ir (to_print ^ s) args
      | (Expr e)::q -> match e with
                | Unit1(Unit0(IntegerExpression(i))) -> aux_print q ir (to_print ^ "%d") (args@[LLVM_i32 i])
                | _ -> let ir2, out = ir_of_expression e sym_tab in
                       aux_print q (ir@@ir2) (to_print ^ "%d") (args@[out])
    in let ir, to_print, args = aux_print a empty_ir "" []
    in let x = newglob(".fmt")
    in let to_print_final, len = string_transform to_print
    in (ir @: llvm_print x len args) @^ llvm_str x len to_print_final

and ir_of_read (a : variable) (sym_tab : symbol_table) : llvm_ir = match a with
    | Var(ident) -> (match uniq_id_of_symbol_table sym_tab ident with
                    | None -> failwith ("Error : unknown var symbol " ^ ident)
                    | Some(id) -> (empty_ir @: llvm_read id))
    | Tab(ident, _) -> failwith "todo : is 'READ tab[i]' valid ?"

and ir_of_if e i io sym_tab : llvm_ir = match io with
    | None -> let ir_e, out = ir_of_expression e sym_tab in
              let out_cond = newtmp() in
              let jump_if = newlab("Then") in
              let jump_endif = newlab("Endif") in
              let ir = ir_of_instruction i sym_tab in
              (((ir_e @: llvm_cmp out_cond out) @: llvm_if (LLVM_var out_cond) jump_if jump_endif) 
              @: llvm_label jump_if)
              @@ ((ir @: llvm_jump jump_endif)
              @: llvm_label jump_endif)
    | Some(instr) -> let ir_e, out = ir_of_expression e sym_tab in
                     let out_cond = newtmp() in
                     let jump_if = newlab("Then") in
                     let jump_else = newlab("Else") in
                     let jump_endif = newlab("Endif") in
                     let ir_then = ir_of_instruction i sym_tab in
                     let ir_else = ir_of_instruction instr sym_tab in
                     (((ir_e @: llvm_cmp out_cond out)
                     @: llvm_if (LLVM_var out_cond) jump_if jump_else) 
                     @: llvm_label jump_if)
                     @@ ((ir_then @: llvm_jump jump_endif) 
                     @: llvm_label jump_else)
                     @@ ((ir_else @: llvm_jump jump_endif)
                     @: llvm_label jump_endif)

and ir_of_while (cond : expression) (bloc : instruction) (sym_tab : symbol_table) : llvm_ir = 
    let ir_cond, out_cond = ir_of_expression cond sym_tab in
    let cond = newtmp() in
    let ir_bloc = ir_of_instruction bloc sym_tab in
    let lab_while = newlab "While" in
    let lab_bloc = newlab "Bloc" in
    let lab_end = newlab "EndWhile" in
    ((empty_ir
    @: llvm_jump lab_while) @: llvm_label lab_while)
    @@ (((ir_cond
    @: llvm_cmp cond out_cond)
    @: llvm_if (LLVM_var cond) lab_bloc lab_end) @: llvm_label lab_bloc)
    @@ (ir_bloc
    @: llvm_jump lab_while) @: llvm_label lab_end

and ir_of_declaration (l : declar list ) (sym_tab : symbol_table) : llvm_ir * symbol_table = match l with 
  |[] -> empty_ir, sym_tab
  |a::q -> let ir, st = aux0_declaration a sym_tab in 
           let ir2, st2 = ir_of_declaration q st in
           ir @@ ir2, st2

and aux0_declaration (dec : declar ) (sym_tab : symbol_table): llvm_ir * symbol_table = match dec with 
  |Declaration([]) -> empty_ir, sym_tab
  |Declaration(a::q) -> let ir, st = aux_declaration a sym_tab in
                        let next, st2 = aux0_declaration (Declaration q) st in
                        ir @@ next, st2

and ir_of_block (b : block) (sym_tab : symbol_table): llvm_ir = match b with 
  |Unit(declar, instr) -> let ir, st = ir_of_declaration declar sym_tab in ir @@ ir_of_instructions instr st
         
and ir_of_call (id : ident) (args : expression list) (sym_tab : symbol_table) : llvm_ir =
  let rec compute_args (args : expression list) (sym_tab : symbol_table) : llvm_ir * (llvm_value list) =
    match args with
    | [] -> empty_ir, []
    | hd::tl -> let ir, arg_value = ir_of_expression hd sym_tab in 
                let ir2, args_values = compute_args tl sym_tab in
                ir @@ ir2, arg_value::args_values
  in let ir, args_values = compute_args args sym_tab in
  ir @: (llvm_call id args_values)

and aux_declaration (var : variable ) (sym_tab : symbol_table) : llvm_ir * symbol_table = match var with 
  |Var(id) -> let un_id = newuniqid id in
              let ir = llvm_declar_var_int ~res_var:un_id ~res_type:LLVM_type_i32 in 
              empty_ir @: ir, (VariableSymbol(Type_Int, id, un_id))::sym_tab 
  |Tab(id, size) -> let un_id = newuniqid id in
                    let ir, out = ir_of_expression size sym_tab in
                    let ir2 = llvm_declar_var_tab ~res_tab:un_id ~res_size:(out) ~res_type:LLVM_type_i32 in 
                    ir @: ir2, (VariableSymbol(Type_Int, id, un_id))::sym_tab 

(* translation from VSL+ types to LLVM types *)
and llvm_type_of_asd_typ : typ -> llvm_type = function
  | Type_Int -> LLVM_type_i32
  | Type_Tab -> LLVM_type_i32_ptr

(* all expressions have type LLVM_type_i32 *)
(* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression (e : expression) (sym_tab : symbol_table) : llvm_ir * llvm_value = match e with
    | AddExpression([]) -> empty_ir, LLVM_i32 0
    | AddExpression (e1::e2) ->
     let ir1, v1 = ir_of_exp_prio_1 e1 sym_tab in
     let ir2, v2 = ir_of_expression (AddExpression e2) sym_tab in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
    | MinusExpression([]) -> empty_ir, LLVM_i32 0
    | MinusExpression (e1::e2) -> 
      let ir1, v1 = ir_of_exp_prio_1 e1 sym_tab in
      let ir2, v2 = ir_of_expression (MinusExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
    | Unit1(e) -> ir_of_exp_prio_1 e sym_tab

and ir_of_exp_prio_1 (e : expPrio1) (sym_tab : symbol_table) : llvm_ir * llvm_value = match e with
    | MulExpression([]) -> empty_ir, LLVM_i32 1
    | MulExpression(e1::e2) ->
      let ir1, v1 = ir_of_exp_prio_0 e1 sym_tab in
      let ir2, v2 = ir_of_exp_prio_1 (MulExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
    | DivExpression([]) -> empty_ir, LLVM_i32 1
    | DivExpression(e1::e2) -> 
      let ir1, v1 = ir_of_exp_prio_0 e1 sym_tab in
      let ir2, v2 = ir_of_exp_prio_1 (DivExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_udiv ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x 
    | Unit0(e) -> ir_of_exp_prio_0 e sym_tab 

and ir_of_exp_prio_0 (e : expPrio0) (sym_tab : symbol_table) : llvm_ir * llvm_value = match e with
  |IntegerExpression i -> empty_ir, LLVM_i32 i
  |ParentheseExpression e -> ir_of_expression e sym_tab
  |CallFun(id, params) -> ir_of_call_expr id params sym_tab
  |TabExpression(id, e) -> (
    match uniq_id_of_symbol_table sym_tab id with
    | None -> failwith ("Error : unknown tab symbol " ^ id)
    | Some(id) -> failwith("TODO : tab in expr")(*
                  let ir, index = ir_of_expression e sym_tab in
                  let out = newtmp() in
                  ir @: (llvm_tab_expr out id), LLVM_var out*)
  )
  |VarExpression e ->
    match uniq_id_of_symbol_table sym_tab e with
    | None -> failwith ("Error : unknown var symbol " ^ e)
    | Some(id) -> let out = newtmp() in
                  empty_ir @: (llvm_var_expr out id), LLVM_var out

and ir_of_call_expr (id : ident) (args : expression list) (sym_tab : symbol_table) : llvm_ir * llvm_value = 
  let rec compute_args (args : expression list) (sym_tab : symbol_table) : llvm_ir * (llvm_value list) =
    match args with
    | [] -> empty_ir, []
    | hd::tl -> let ir, arg_value = ir_of_expression hd sym_tab in 
                let ir2, args_values = compute_args tl sym_tab in
                ir @@ ir2, arg_value::args_values
  in let ir, args_values = compute_args args sym_tab in
  let fun_dest = newtmp() in
  ir @: (llvm_call_expr fun_dest id args_values), LLVM_var fun_dest