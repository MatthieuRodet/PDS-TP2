open ASD
open Llvm
open Utils
open SymbolTable



(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir = 
  (*let ir, v = ir_of_prog prog in*)
  let ir, sym_tab, called_func = ir_of_prog prog [] [] in 
  (*let v = LLVM_i32(0) in*) 
  (* adds the return instruction *)
  (*let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in*)
  (* We create the function main *)
  (*let ir = llvm_define_main ir in*)
  let _ = verify_proto_declaration sym_tab called_func in
  ir

and verify_proto_declaration (sym_tab : symbol_table) (called_func : ident list): unit =
  match sym_tab with
  | [] -> ()
  | _ -> ()

and ir_of_prog (prog :  program) (sym_tab : symbol_table) (called_func : ident list): llvm_ir * symbol_table * (ident list) = match prog with 
  |Prog([]) ->  empty_ir, [], called_func
  |Prog(a::q) -> let ir, decl, called = ir_of_fun a sym_tab in
                 let ir2, decl2, called2 = ir_of_prog (Prog q) ((FunctionSymbol decl)::sym_tab) (called @ called_func) in
                 ir @@ ir2, (FunctionSymbol decl)::decl2, called2

and ir_of_fun (f : func) (sym_tab : symbol_table) : llvm_ir * function_symbol * (ident list) = match f with
  |Proto(ret, id, args) -> (match lookup sym_tab id with
    | None -> empty_ir, {return_type=ret; identifier=id; arguments=args; state=Declared}, []
    | _ -> failwith("Error : Prototype of the already declared function " ^ id)
    )
  |Func(ret, id, args, body) -> (match lookup sym_tab id with
    | Some(FunctionSymbol {state=Defined; _}) -> failwith("Error : Redefinition of the already defined function " ^ id)
    | Some(FunctionSymbol {return_type=ret2; arguments=args2; _}) -> 
      (if not (verify_fun_compatibility ret ret2 args args2) then 
        failwith("Error : Incompatible prototype and definition of the function " ^ id)
      else
        let fun_head = llvm_fun_header (ret_type_conv ret) id (params_to_args args) in
        let arg_ir, arg_sym_tab = sym_tab_of_args args sym_tab in
        match ret with
        | T_Void -> 
          let body_ir, called = ir_of_instruction body (arg_sym_tab @ sym_tab) in
          (empty_ir @: fun_head) @@ arg_ir @@ body_ir @: "ret void\n}\n",
          {return_type=ret; identifier=id; arguments=args; state=Defined},
          called
        | T_Int ->
          let body_ir, called = ir_of_instruction body (arg_sym_tab @ sym_tab) in
          (empty_ir @: fun_head) @@ arg_ir @@ body_ir @: "ret i32 0\n}\n",
          {return_type=ret; identifier=id; arguments=args; state=Defined},
          called
          )
    | Some(VariableSymbol _) -> failwith("Error : Function defined with the name of a variable : " ^ id)
    | None -> let fun_head = llvm_fun_header (ret_type_conv ret) id (params_to_args args) in
          let arg_ir, arg_sym_tab = sym_tab_of_args args sym_tab in
          match ret with
          | T_Void -> 
            let body_ir, called = ir_of_instruction body (arg_sym_tab @ sym_tab) in
            (empty_ir @: fun_head) @@ arg_ir @@ body_ir @: "ret void\n}\n",
            {return_type=ret; identifier=id; arguments=args; state=Defined},
            called
          | T_Int ->
            let body_ir, called = ir_of_instruction body (arg_sym_tab @ sym_tab) in
            (empty_ir @: fun_head) @@ arg_ir @@ body_ir @: "ret i32 0\n}\n",
            {return_type=ret; identifier=id; arguments=args; state=Defined},
            called
    )

and sym_tab_of_args (args : params list) (sym_tab : symbol_table) : llvm_ir * symbol_table =
  match args with
  | [] -> empty_ir, []
  | (Var_params id)::tl -> let uniqid = newuniqid id in
                           let ir, st = sym_tab_of_args tl sym_tab in 
                           (ir @: llvm_declar_var_int uniqid LLVM_type_i32) @: llvm_affect_var (LLVM_var id) uniqid, VariableSymbol(Type_Int, id, uniqid)::st
  | (Tab_params id)::tl -> let ir, st = sym_tab_of_args tl sym_tab in
                           ir, VariableSymbol(Type_Tab_Ptr, id, id)::st

and verify_fun_compatibility (ret : ret_type) (ret2 : ret_type) (args : params list) (args2 : params list) : bool =
    let rec check_args args args2 = match args, args2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | (Var_params _)::tl1, Var_params(_)::tl2 -> true && check_args tl1 tl2
    | (Tab_params _)::tl1, Tab_params(_)::tl2 -> true && check_args tl1 tl2
    | _ -> false
  in ret == ret2 && check_args args args2

(*
and ir_of_main (body : instruction) (sym_tab : symbol_table) : llvm_ir =
  let ir = ir_of_instruction body sym_tab in
  llvm_define_main ir
*)
and ret_type_conv ret_type = match ret_type with
  | T_Int -> LLVM_type_i32
  | T_Void -> LLVM_type_void
and params_to_args (params : params list) =
  match params with
  | [] -> []
  | (Var_params id)::tl -> (LLVM_type_i32, id)::params_to_args tl
  | (Tab_params id)::tl -> (LLVM_type_i32_ptr, id)::params_to_args tl
and ir_of_instructions ( l : instruction list) (sym_tab : symbol_table) (called_func : ident list) : llvm_ir * ident list = match l with 
  |[] -> empty_ir, called_func
  |[a] -> let ir, called = ir_of_instruction a sym_tab in ir, called @ called_func
  |a::q -> let ir, called = ir_of_instruction a sym_tab in
           let ir2, called2 = ir_of_instructions q sym_tab (called @ called_func) in
           ir @@ ir2, called2

and ir_of_instruction  (instr : instruction) (sym_tab : symbol_table) : llvm_ir * (ident list) = match instr with 
  |Affect(Var(v),e) -> let ir, out, called = ir_of_expression e sym_tab in
                                 ir @: ir_of_affect_var v out sym_tab, called
  |Affect(Tab(v, index),e) -> let ir, index_value, called = ir_of_expression index sym_tab in
                              let ir2, expr_value, called2 = ir_of_expression e sym_tab in
                              let ptr_instr, ptr = instr_to_get_ptr v index_value sym_tab in
                              let store_instr = llvm_affect_var expr_value ptr in
                              (ir @@ ir2 @: ptr_instr) @: store_instr, called @ called2
  |Print([]) -> empty_ir, []
  |Print(items) -> ir_of_print items sym_tab
  |Read([]) -> empty_ir, []
  |Read(a::q) ->  let ir, called = ir_of_read a sym_tab in
                  let ir2, called2 = ir_of_instruction (Read q) sym_tab in
                  ir @@ ir2, called @ called2
  |If(e,i,io) -> ir_of_if e i io sym_tab
  |While(e,i) -> ir_of_while e i sym_tab
  |Block(b) -> ir_of_block b sym_tab
  |Ret(e) -> let ir, ret_val, called = ir_of_expression e sym_tab in ir @: llvm_ret ret_val, called
  |Call(id, args) -> ir_of_call id args sym_tab

and ir_of_affect_var (var : ident) (value : llvm_value) (sym_tab : symbol_table): llvm_instr =
  match uniq_id_of_symbol_table sym_tab var with
  | None -> failwith ("Error : unknown var symbol " ^ var)
  | Some(Type_Tab _, id) -> failwith ("Error : tab symbol " ^ var ^ " used as var symbol")
  | Some(Type_Tab_Ptr, id) -> failwith ("Error : tab symbol " ^ var ^ " used as var symbol")
  | Some(Type_Int, uniq_id) -> llvm_affect_var value uniq_id

and ir_of_print (a : item list) (sym_tab : symbol_table) : llvm_ir * (ident list) = 
    let rec aux_print (a : item list) (ir : llvm_ir) (to_print : string) (args : llvm_value list) (called_func : ident list): llvm_ir * string * llvm_value list * ident list=
      match a with
      | [] -> ir, to_print, args, called_func
      | (Str s)::q -> aux_print q ir (to_print ^ s) args called_func
      | (Expr e)::q -> match e with
                | Unit1(Unit0(IntegerExpression(i))) -> aux_print q ir (to_print ^ "%d") (args@[LLVM_i32 i]) called_func
                | _ -> let ir2, out, called = ir_of_expression e sym_tab in
                       aux_print q (ir@@ir2) (to_print ^ "%d") (args@[out]) (called @ called_func)
    in let ir, to_print, args, called = aux_print a empty_ir "" [] []
    in let x = newglob(".fmt")
    in let to_print_final, len = string_transform to_print
    in (ir @: llvm_print x len args) @^ llvm_str x len to_print_final, called

and ir_of_read (a : variable) (sym_tab : symbol_table) : llvm_ir * ident list = match a with
    | Var(ident) -> (match uniq_id_of_symbol_table sym_tab ident with
                    | None -> failwith ("Error : unknown var symbol " ^ ident)
                    | Some(Type_Tab _, id) -> failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
                    | Some(Type_Tab_Ptr, id) -> failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
                    | Some(Type_Int, id) -> (empty_ir @: llvm_read id)), []
    | Tab(ident, index) -> (match uniq_id_of_symbol_table sym_tab ident with
                    | None -> failwith ("Error : unknown var symbol " ^ ident)
                    | Some(Type_Int, id) -> failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
                    | Some(_, id) ->
                              let ir, index_value, called = ir_of_expression index sym_tab in
                              let ptr_instr, ptr = instr_to_get_ptr ident index_value sym_tab in
                              (ir @: ptr_instr) @: llvm_read ptr, called
                    )
and ir_of_if e i io sym_tab : llvm_ir * ident list = match io with
    | None -> let ir_e, out, called = ir_of_expression e sym_tab in
              let out_cond = newtmp() in
              let jump_if = newlab("Then") in
              let jump_endif = newlab("Endif") in
              let ir, called2 = ir_of_instruction i sym_tab in
              (((ir_e @: llvm_cmp out_cond out) @: llvm_if (LLVM_var out_cond) jump_if jump_endif) 
              @: llvm_label jump_if)
              @@ ((ir @: llvm_jump jump_endif)
              @: llvm_label jump_endif),
              called @ called2
    | Some(instr) -> let ir_e, out, called = ir_of_expression e sym_tab in
                     let out_cond = newtmp() in
                     let jump_if = newlab("Then") in
                     let jump_else = newlab("Else") in
                     let jump_endif = newlab("Endif") in
                     let ir_then, called2 = ir_of_instruction i sym_tab in
                     let ir_else, called3 = ir_of_instruction instr sym_tab in
                     (((ir_e @: llvm_cmp out_cond out)
                     @: llvm_if (LLVM_var out_cond) jump_if jump_else) 
                     @: llvm_label jump_if)
                     @@ ((ir_then @: llvm_jump jump_endif) 
                     @: llvm_label jump_else)
                     @@ ((ir_else @: llvm_jump jump_endif)
                     @: llvm_label jump_endif),
                     called @ called2 @ called3

and ir_of_while (cond : expression) (bloc : instruction) (sym_tab : symbol_table) : llvm_ir * ident list = 
    let ir_cond, out_cond, called = ir_of_expression cond sym_tab in
    let cond = newtmp() in
    let ir_bloc, called2 = ir_of_instruction bloc sym_tab in
    let lab_while = newlab "While" in
    let lab_bloc = newlab "Bloc" in
    let lab_end = newlab "EndWhile" in
    ((empty_ir
    @: llvm_jump lab_while) @: llvm_label lab_while)
    @@ (((ir_cond
    @: llvm_cmp cond out_cond)
    @: llvm_if (LLVM_var cond) lab_bloc lab_end) @: llvm_label lab_bloc)
    @@ (ir_bloc
    @: llvm_jump lab_while) @: llvm_label lab_end,
    called @ called2

and ir_of_declaration (l : declar list ) (sym_tab : symbol_table) : llvm_ir * symbol_table = match l with 
  |[] -> empty_ir, sym_tab
  |a::q -> let ir, st = aux0_declaration a sym_tab in 
           let ir2, st2 = ir_of_declaration q st in
           ir @@ ir2, st2

and aux0_declaration (dec : declar ) (sym_tab : symbol_table): llvm_ir * symbol_table = match dec with 
  |Declaration([]) -> empty_ir, sym_tab
  |Declaration(a::q) -> let ir, st = aux_declaration a sym_tab in
                        let next, st2 = aux0_declaration (Declaration q) st in
                        (empty_ir @: ir) @@ next, st2

and ir_of_block (b : block) (sym_tab : symbol_table): llvm_ir * ident list = match b with 
  |Unit(declar, instr) -> let ir, st = ir_of_declaration declar [] in
                          let ir2, called = ir_of_instructions instr (st@sym_tab) [] in
                          ir @@ ir2, called
         
and ir_of_call (id : ident) (args : expression list) (sym_tab : symbol_table) : llvm_ir * ident list =
  let rec compute_args (args : expression list) (sym_tab : symbol_table) (called_func : ident list) : llvm_ir * (llvm_type * llvm_value) list * ident list =
    match args with
    | [] -> empty_ir, [], called_func
    | (Unit1(Unit0(VarExpression id)))::tl -> (match uniq_id_of_symbol_table sym_tab id with
                | Some(typ, un_id) when typ != Type_Int -> 
                  let ptr_instr, out = instr_to_get_ptr id (LLVM_i32 0) sym_tab in
                  let ir2, args_values, called = compute_args tl sym_tab called_func in
                  (empty_ir @: ptr_instr) @@ ir2, (LLVM_type_i32_ptr, (LLVM_var out))::args_values, called
                | _ -> let ir, arg_value, called = ir_of_expression (Unit1(Unit0(VarExpression id))) sym_tab in 
                       let ir2, args_values, called2 = compute_args tl sym_tab (called @ called_func) in
                       ir @@ ir2, (LLVM_type_i32, arg_value)::args_values, called2)
    | hd::tl -> let ir, arg_value, called = ir_of_expression hd sym_tab in 
                let ir2, args_values, called2 = compute_args tl sym_tab (called @ called_func) in
                ir @@ ir2, (LLVM_type_i32, arg_value)::args_values, called2
  
  in match lookup sym_tab id with
  | None -> failwith("Error : Call of the undefined function " ^ id)
  | Some(VariableSymbol _) -> failwith("Error : Var symbol " ^ id ^ " is not callable")
  | Some(FunctionSymbol {arguments=args2}) -> if verify_fun_call_compatibility T_Int T_Int args args2 sym_tab then
                                                let ir, args_values, called = compute_args args sym_tab [] in
                                                ir @: (llvm_call id args_values), id::called
                                              else
                                                failwith("Error : Incompatible arguments type and/or number in declaration and call of function " ^ id)  

and aux_declaration (var : decl_variable ) (sym_tab : symbol_table) : llvm_instr * symbol_table = match var with 
  |DVar(id)-> if lookup sym_tab id != None then
                failwith("Error : multiple déclaration of " ^ id)
              else
                let un_id = newuniqid id in
                let ir = llvm_declar_var_int ~res_var:un_id ~res_type:LLVM_type_i32 in 
                ir, (VariableSymbol(Type_Int, id, un_id))::sym_tab 
  |DTab(id, size)-> if lookup sym_tab id != None then
                      failwith("Error : multiple déclaration of " ^ id)
                    else
                      let un_id = newuniqid id in
                      let ir = llvm_declar_var_tab ~res_tab:un_id ~res_size:(size) ~res_type:LLVM_type_i32 in 
                      ir, (VariableSymbol(Type_Tab size, id, un_id))::sym_tab 

(* translation from VSL+ types to LLVM types *)
and llvm_type_of_asd_typ : typ -> llvm_type = function
  | Type_Int -> LLVM_type_i32
  | Type_Tab(x) -> LLVM_type_i32_ptr
  | Type_Tab_Ptr -> LLVM_type_i32_ptr

(* all expressions have type LLVM_type_i32 *)
(* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression (e : expression) (sym_tab : symbol_table) : llvm_ir * llvm_value * ident list = match e with
    | AddExpression([]) -> empty_ir, LLVM_i32 0, []
    | AddExpression (e1::e2) ->
     let ir1, v1, called = ir_of_exp_prio_1 e1 sym_tab in
     let ir2, v2, called2 = ir_of_expression (AddExpression e2) sym_tab in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x, called2 @ called 
    | MinusExpression([]) -> empty_ir, LLVM_i32 0, []
    | MinusExpression (e1::e2) -> 
      let ir1, v1, called = ir_of_exp_prio_1 e1 sym_tab in
      let ir2, v2, called2 = ir_of_expression (MinusExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x, called @ called2
    | Unit1(e) -> ir_of_exp_prio_1 e sym_tab

and ir_of_exp_prio_1 (e : expPrio1) (sym_tab : symbol_table) : llvm_ir * llvm_value * ident list = match e with
    | MulExpression([]) -> empty_ir, LLVM_i32 1, []
    | MulExpression(e1::e2) ->
      let ir1, v1, called = ir_of_exp_prio_0 e1 sym_tab in
      let ir2, v2, called2 = ir_of_exp_prio_1 (MulExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x, called @ called2
    | DivExpression([]) -> empty_ir, LLVM_i32 1, []
    | DivExpression(e1::e2) -> 
      let ir1, v1, called = ir_of_exp_prio_0 e1 sym_tab in
      let ir2, v2, called2 = ir_of_exp_prio_1 (DivExpression e2) sym_tab in
      let x = newtmp () in 
      let ir = ir1 @@ ir2 @: llvm_udiv ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in 
      ir, LLVM_var x, called @ called2
    | Unit0(e) -> ir_of_exp_prio_0 e sym_tab 

and ir_of_exp_prio_0 (e : expPrio0) (sym_tab : symbol_table) : llvm_ir * llvm_value * ident list = match e with
  |IntegerExpression i -> empty_ir, LLVM_i32 i, []
  |ParentheseExpression e -> ir_of_expression e sym_tab
  |CallFun(id, params) -> ir_of_call_expr id params sym_tab []
  |TabExpression(id, e) -> ( 
                  let ir, index, called = ir_of_expression e sym_tab in
                  let out = newtmp() in
                  let instr1, ptr = instr_to_get_ptr id index sym_tab in
                  let instr2 = llvm_var_expr out ptr in
                  (ir @: instr1) @: instr2, LLVM_var out, called
  )
  |VarExpression e ->
    match uniq_id_of_symbol_table sym_tab e with
    | None -> failwith ("Error : unknown var symbol " ^ e)
    | Some(Type_Tab _, _) -> failwith("Error : tab symbol " ^ e ^ " used as var symbol")
    | Some(Type_Tab_Ptr, _) -> failwith("Error : tab symbol " ^ e ^ " used as var symbol")
    | Some(typ, uniq_id) -> let out = newtmp() in
                  empty_ir @: (llvm_var_expr out uniq_id), LLVM_var out, []

and ir_of_call_expr (id : ident) (args : expression list) (sym_tab : symbol_table) (called_func : ident list): llvm_ir * llvm_value * ident list = 
let rec compute_args (args : expression list) (sym_tab : symbol_table) (called_func : ident list) : llvm_ir * (llvm_type * llvm_value) list * ident list =
  match args with
  | [] -> empty_ir, [], called_func
  | (Unit1(Unit0(VarExpression id)))::tl -> (match uniq_id_of_symbol_table sym_tab id with
              | Some(typ, un_id) when typ != Type_Int -> 
                let ptr_instr, out = instr_to_get_ptr id (LLVM_i32 0) sym_tab in
                let ir2, args_values, called = compute_args tl sym_tab called_func in
                (empty_ir @: ptr_instr) @@ ir2, (LLVM_type_i32_ptr, (LLVM_var out))::args_values, called
              | _ -> let ir, arg_value, called = ir_of_expression (Unit1(Unit0(VarExpression id))) sym_tab in 
                     let ir2, args_values, called2 = compute_args tl sym_tab (called @ called_func) in
                     ir @@ ir2, (LLVM_type_i32, arg_value)::args_values, called2)
  | hd::tl -> let ir, arg_value, called = ir_of_expression hd sym_tab in 
              let ir2, args_values, called2 = compute_args tl sym_tab (called @ called_func) in
              ir @@ ir2, (LLVM_type_i32, arg_value)::args_values, called2
  in match lookup sym_tab id with
  | None -> failwith("Error : Call of the undefined function " ^ id)
  | Some(VariableSymbol _) -> failwith("Error : Var symbol " ^ id ^ " is not callable")
  | Some(FunctionSymbol {return_type=T_Void}) -> failwith("Error : Void type function (" ^ id ^ ") cannot be called in an expression")
  | Some(FunctionSymbol {arguments=args2}) -> if verify_fun_call_compatibility T_Int T_Int args args2 sym_tab then
                                                let ir, args_values, called = compute_args args sym_tab called_func in
                                                let fun_dest = newtmp() in
                                                ir @: (llvm_call_expr fun_dest id args_values), LLVM_var fun_dest, called
                                              else
                                                failwith("Error : Incompatible arguments type and/or number in declaration and call of function " ^ id)  



and verify_fun_call_compatibility (ret : ret_type) (ret2 : ret_type) (args : expression list) (args2 : params list) (sym_tab : symbol_table) : bool =
  let rec check_args args args2 = match args, args2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | (Unit1(Unit0(VarExpression id)))::tl1, Tab_params(_)::tl2  -> (
            match lookup sym_tab id with
            | Some(VariableSymbol(Type_Tab _, _, _)) -> true && check_args tl1 tl2
            | Some(VariableSymbol(Type_Tab_Ptr, _, _)) -> true && check_args tl1 tl2
            | _ -> false
            )
    | (Unit1(Unit0(VarExpression id)))::tl1, Var_params(_)::tl2  -> (
            match lookup sym_tab id with
            | Some(VariableSymbol(Type_Int, _, _)) -> true && check_args tl1 tl2
            | _ -> false
            )
    | _::tl1, Var_params(_)::tl2 -> true && check_args tl1 tl2
    | _ -> false
  in ret == ret2 && check_args args args2

and instr_to_get_ptr tab index sym_tab : llvm_instr * llvm_var =
  match uniq_id_of_symbol_table sym_tab tab with
  | None -> failwith("Error : unknown tab symbol " ^ tab)
  | Some(Type_Int, uniq_id) -> failwith("Error : tab symbol " ^ tab ^ " used as var symbol")
  | Some(Type_Tab size, uniq_id) -> let ptr = newtmp() in
                                    llvm_tab_expr ptr uniq_id index size, ptr
  | Some(Type_Tab_Ptr, uniq_id) -> let ptr = newtmp() in
                                   llvm_ptr_expr ptr uniq_id index, ptr
