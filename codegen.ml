open ASD
open Llvm
open Utils
open SymbolTable

(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir =
    let ir, sym_tab, called_func = ir_of_prog prog [] [] in
    let _ = verify_proto_declaration sym_tab called_func in
    ir

(* Verify that all prototyped and called functions have been defined *)
and verify_proto_declaration (sym_tab : symbol_table) (called_func : ident list)
        : unit =
    match called_func with
    | [] -> ()
    | id :: t -> (
        match lookup sym_tab id with
        | Some (FunctionSymbol { state = Declared }) ->
            failwith ("Call of the declared but undefined function " ^ id)
        | _ -> verify_proto_declaration sym_tab t)

and ir_of_prog (prog : program) (sym_tab : symbol_table)
        (called_func : ident list) :
        llvm_ir * symbol_table * ident list =
    match prog with
    | Prog [] -> (empty_ir, sym_tab, called_func)
    | Prog (a :: q) ->
        let ir, decl, called = ir_of_fun a sym_tab in
        let ir2, decl2, called2 =
            ir_of_prog (Prog q) decl (called @ called_func)
        in
        (ir @@ ir2, decl2, called2)

and ir_of_fun (f : func) (sym_tab : symbol_table) :
        llvm_ir * symbol_table * ident list =
    match f with
    | Proto (ret, id, args) ->
        if lookup sym_tab id != None then
            failwith ("Error : Prototype of the already declared function " ^ id)
        else
            ( empty_ir,
                FunctionSymbol
                    {
                        return_type = ret;
                        identifier = id;
                        arguments = args;
                        state = Declared;
                    }
                :: sym_tab,
                [] )
    | Func (ret, id, args, body) -> (
        match lookup sym_tab id with
        | Some (VariableSymbol _) ->
            failwith
                ("Error : Function defined with the name of a variable : " ^ id)
        | Some (FunctionSymbol { state = Defined; _ }) ->
            failwith ("Error : Redefinition of the already defined function " ^ id)
        | Some (FunctionSymbol { return_type = ret2; arguments = args2; _ }) ->
            if not (verify_fun_compatibility ret ret2 args args2) then
                failwith
                    ("Error : Incompatible prototype and definition of the function "
                    ^ id)
            else ir_of_func ret id args body (remove_proto id sym_tab)
        | None -> ir_of_func ret id args body sym_tab)

and ir_of_func ret id args body sym_tab :
        llvm_ir * symbol_table * ident list =
    let st =
        FunctionSymbol
            { return_type = ret; identifier = id; arguments = args; state = Defined }
        :: sym_tab
    in
    let struct_args = llvm_struct_of_args id (type_of_args args) in
    let fun_head = llvm_fun_header id in
    let arg_ir, arg_sym_tab = sym_tab_of_args id args in
    let body_ir, called = ir_of_instruction body (arg_sym_tab @ st) in
    ( ((empty_ir @^ struct_args) @: fun_head)
        @@ arg_ir @@ body_ir @: "ret i8* null\n}\n\n",
        st,
        called )

and remove_proto (id : ident) (sym_tab : symbol_table) :
        symbol_table =
    match sym_tab with
    | [] -> []
    | FunctionSymbol { identifier = id; state = Declared; _ } :: tl -> tl
    | hd :: tl -> hd :: remove_proto id tl

and type_of_args (args : params list) :
        llvm_type list =
    match args with
    | [] -> []
    | Var_params _ :: tl -> LLVM_type_i32 :: type_of_args tl
    | Tab_params _ :: tl -> LLVM_type_i32_ptr :: type_of_args tl

and sym_tab_of_args (fun_id : ident) (args : params list) :
        llvm_ir * symbol_table =
    let rec aux fun_id struct_id args index : llvm_ir * symbol_table =
        match args with
        | [] -> (empty_ir, [])
        | Var_params id :: tl ->
            let uniqid = newuniqid id in
            let ir, st = aux fun_id struct_id tl (index + 1) in
            ( (empty_ir @: llvm_extract_var_arg uniqid fun_id struct_id index) @@ ir,
                VariableSymbol (Type_Int, id, uniqid) :: st )
        | Tab_params id :: tl ->
            let uniqid = newuniqid id in
            let ir, st = aux fun_id struct_id tl (index + 1) in
            ( (empty_ir @: llvm_extract_tab_arg uniqid fun_id struct_id index) @@ ir,
                VariableSymbol (Type_Tab_Ptr, id, uniqid) :: st )
    in
    let out = newtmp () in
    let ir, st = aux fun_id out args 0 in
    ((empty_ir @: llvm_args_struct out fun_id) @@ ir, st)

and verify_fun_compatibility (ret : ret_type) (ret2 : ret_type)
        (args : params list) (args2 : params list) :
        bool =
    let rec check_args args args2 =
        match (args, args2) with
        | [], [] -> true
        | [], _ -> false
        | _, [] -> false
        | Var_params _ :: tl1, Var_params _ :: tl2 -> true && check_args tl1 tl2
        | Tab_params _ :: tl1, Tab_params _ :: tl2 -> true && check_args tl1 tl2
        | _ -> false
    in
    ret == ret2 && check_args args args2

and ret_type_conv (ret_type : ret_type) :
        llvm_type =
    match ret_type with T_Int -> LLVM_type_i32 | T_Void -> LLVM_type_void

and params_to_args (params : params list) :
        (llvm_type * ident) list =
    match params with
    | [] -> []
    | Var_params id :: tl -> (LLVM_type_i32, id) :: params_to_args tl
    | Tab_params id :: tl -> (LLVM_type_i32_ptr, id) :: params_to_args tl

and ir_of_instructions (l : instruction list) (sym_tab : symbol_table)
        (called_func : ident list) :
        llvm_ir * ident list =
    match l with
    | [] -> (empty_ir, called_func)
    | [ a ] ->
        let ir, called = ir_of_instruction a sym_tab in
        (ir, called @ called_func)
    | a :: q ->
        let ir, called = ir_of_instruction a sym_tab in
        let ir2, called2 = ir_of_instructions q sym_tab (called @ called_func) in
        (ir @@ ir2, called2)

and ir_of_instruction (instr : instruction) (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match instr with
    | Affect (Var v, e) ->
        let ir, out, called = ir_of_expression e sym_tab in
        (ir @: ir_of_affect_var v out sym_tab, called)
    | Affect (Tab (v, index), e) ->
        let ir, index_value, called = ir_of_expression index sym_tab in
        let ir2, expr_value, called2 = ir_of_expression e sym_tab in
        let ptr_instr, ptr = instr_to_get_ptr v index_value sym_tab in
        let store_instr = llvm_affect_var expr_value ptr in
        ((ir @@ ir2 @: ptr_instr) @: store_instr, called @ called2)
    | Print [] -> (empty_ir, [])
    | Print items -> ir_of_print items sym_tab
    | Read [] -> (empty_ir, [])
    | Read (a :: q) ->
        let ir, called = ir_of_read a sym_tab in
        let ir2, called2 = ir_of_instruction (Read q) sym_tab in
        (ir @@ ir2, called @ called2)
    | If (e, i, io) -> ir_of_if e i io sym_tab
    | While (e, i) -> ir_of_while e i sym_tab
    | Block b -> ir_of_block b sym_tab
    | Ret e ->
        let ir, ret_val, called = ir_of_expression e sym_tab in
        (ir @: llvm_ret ret_val, called)
    | Call (id, args) -> ir_of_call id args sym_tab
    | Thread (tid, fun_id, args) -> ir_of_thread tid fun_id args sym_tab
    | MapRed (tab, cut_count, tab_size, fun_id, args) ->
        let tids, ir, called2 =
            ir_of_map_red tab cut_count tab_size fun_id args sym_tab
        in
        let ir2 = ir_of_join_map_red tids in
        (ir @@ ir2, called2)
    | Join (tid, None) -> (
        match uniq_id_of_symbol_table sym_tab tid with
        | Some (Type_Tid, uniq_id) -> (empty_ir @: llvm_join_void uniq_id, [])
        | _ -> failwith "Error : JOIN first argument must be of type TID")
    | Join (tid, Some res_id) -> (
        match
            ( uniq_id_of_symbol_table sym_tab tid,
                uniq_id_of_symbol_table sym_tab res_id )
        with
        | Some (Type_Tid, uniq_id), Some (Type_Int, res_uniq_id) ->
            (empty_ir @: llvm_join_int uniq_id res_uniq_id, [])
        | _ -> failwith "Error : JOIN first argument must be of type TID")

and ir_of_affect_var (var : ident) (value : llvm_value) (sym_tab : symbol_table) :
        llvm_instr =
    match uniq_id_of_symbol_table sym_tab var with
    | None -> failwith ("Error : unknown var symbol " ^ var)
    | Some (Type_Tab _, id) ->
        failwith ("Error : tab symbol " ^ var ^ " used as var symbol")
    | Some (Type_Tab_Ptr, id) ->
        failwith ("Error : tab symbol " ^ var ^ " used as var symbol")
    | Some (Type_Tid, id) ->
        failwith ("Error : TID symbol " ^ var ^ " used as var symbol")
    | Some (Type_Int, uniq_id) -> llvm_affect_var value uniq_id

and ir_of_thread (tid : ident) (fun_id : ident) (args : expression list)
        (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match lookup sym_tab fun_id with
    | None -> failwith "Error : unknow function in thread initialization "
    | Some (VariableSymbol _) ->
        failwith ("Error : Variabe symbol " ^ fun_id ^ "is not callable ")
    | Some (FunctionSymbol { arguments = args2; _ }) -> (
        match uniq_id_of_symbol_table sym_tab tid with
        | Some (Type_Tid, uniq_id) ->
            let ir, args_struct, called = compute_args fun_id args sym_tab [] in
            (ir @: llvm_create_thread uniq_id fun_id args_struct, fun_id :: called)
        | _ -> failwith "Error : THREAD first argument must be of type TID")

and ir_of_print (a : item list) (sym_tab : symbol_table) :
        llvm_ir * ident list =
    let rec aux_print (a : item list) (ir : llvm_ir) (to_print : string)
            (args : llvm_value list) (called_func : ident list) :
            llvm_ir * string * llvm_value list * ident list =
        match a with
        | [] -> (ir, to_print, args, called_func)
        | Str s :: q -> aux_print q ir (to_print ^ s) args called_func
        | Expr e :: q -> (
            match e with
            | Unit1 (Unit0 (IntegerExpression i)) ->
                aux_print q ir (to_print ^ "%d") (args @ [ LLVM_i32 i ]) called_func
            | _ ->
                let ir2, out, called = ir_of_expression e sym_tab in
                aux_print q (ir @@ ir2) (to_print ^ "%d")
                    (args @ [ out ])
                    (called @ called_func))
    in
    let ir, to_print, args, called = aux_print a empty_ir "" [] [] in
    let x = newglob ".fmt" in
    let to_print_final, len = string_transform to_print in
    ((ir @: llvm_print x len args) @^ llvm_str x len to_print_final, called)

and ir_of_read (a : variable) (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match a with
    | Var ident ->
        ( (match uniq_id_of_symbol_table sym_tab ident with
            | None -> failwith ("Error : unknown var symbol " ^ ident)
            | Some (Type_Tab _, id) ->
                failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
            | Some (Type_Tab_Ptr, id) ->
                failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
            | Some (Type_Tid, id) ->
                failwith ("Error : TID symbol " ^ ident ^ " used as var symbol")
            | Some (Type_Int, id) -> empty_ir @: llvm_read id),
            [] )
    | Tab (ident, index) -> (
        match uniq_id_of_symbol_table sym_tab ident with
        | None -> failwith ("Error : unknown var symbol " ^ ident)
        | Some (Type_Int, id) ->
            failwith ("Error : tab symbol " ^ ident ^ " used as var symbol")
        | Some (_, id) ->
            let ir, index_value, called = ir_of_expression index sym_tab in
            let ptr_instr, ptr = instr_to_get_ptr ident index_value sym_tab in
            ((ir @: ptr_instr) @: llvm_read ptr, called))

and ir_of_if (e : expression) (i : instruction) (io : instruction option)
        (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match io with
    | None ->
        let ir_e, out, called = ir_of_expression e sym_tab in
        let out_cond = newtmp () in
        let jump_if = newlab "Then" in
        let jump_endif = newlab "Endif" in
        let ir, called2 = ir_of_instruction i sym_tab in
        ( (((ir_e @: llvm_cmp out_cond out)
                @: llvm_if (LLVM_var out_cond) jump_if jump_endif)
            @: llvm_label jump_if)
            @@ (ir @: llvm_jump jump_endif)
            @: llvm_label jump_endif,
            called @ called2 )
    | Some instr ->
        let ir_e, out, called = ir_of_expression e sym_tab in
        let out_cond = newtmp () in
        let jump_if = newlab "Then" in
        let jump_else = newlab "Else" in
        let jump_endif = newlab "Endif" in
        let ir_then, called2 = ir_of_instruction i sym_tab in
        let ir_else, called3 = ir_of_instruction instr sym_tab in
        ( (((ir_e @: llvm_cmp out_cond out)
                @: llvm_if (LLVM_var out_cond) jump_if jump_else)
            @: llvm_label jump_if)
            @@ ((ir_then @: llvm_jump jump_endif) @: llvm_label jump_else)
            @@ (ir_else @: llvm_jump jump_endif)
            @: llvm_label jump_endif,
            called @ called2 @ called3 )

and ir_of_while (cond : expression) (bloc : instruction)
        (sym_tab : symbol_table) :
        llvm_ir * ident list =
    let ir_cond, out_cond, called = ir_of_expression cond sym_tab in
    let cond = newtmp () in
    let ir_bloc, called2 = ir_of_instruction bloc sym_tab in
    let lab_while = newlab "While" in
    let lab_bloc = newlab "Bloc" in
    let lab_end = newlab "EndWhile" in
    ( ((empty_ir @: llvm_jump lab_while) @: llvm_label lab_while)
        @@ (((ir_cond @: llvm_cmp cond out_cond)
                @: llvm_if (LLVM_var cond) lab_bloc lab_end)
             @: llvm_label lab_bloc)
        @@ (ir_bloc @: llvm_jump lab_while)
        @: llvm_label lab_end,
        called @ called2 )

and ir_of_declaration (l : declar list) (sym_tab : symbol_table) :
        llvm_ir * symbol_table =
    match l with
    | [] -> (empty_ir, sym_tab)
    | a :: q ->
        let ir, st = aux0_declaration a sym_tab in
        let ir2, st2 = ir_of_declaration q st in
        (ir @@ ir2, st2)

and aux0_declaration (dec : declar) (sym_tab : symbol_table) :
        llvm_ir * symbol_table =
    match dec with
    | Declaration [] -> (empty_ir, sym_tab)
    | Declaration (a :: q) ->
        let ir, st = aux_declaration a sym_tab in
        let next, st2 = aux0_declaration (Declaration q) st in
        ((empty_ir @: ir) @@ next, st2)
    | DeclarationTid [] -> (empty_ir, sym_tab)
    | DeclarationTid (a :: q) ->
        let ir, st = aux_declaration_tid a sym_tab in
        let next, st2 = aux0_declaration (DeclarationTid q) st in
        ((empty_ir @: ir) @@ next, st2)

and ir_of_block (b : block) (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match b with
    | Unit (declar, instr) ->
        let ir, st = ir_of_declaration declar [] in
        let ir2, called = ir_of_instructions instr (st @ sym_tab) [] in
        (ir @@ ir2, called)

and ir_of_call (id : ident) (args : expression list) (sym_tab : symbol_table) :
        llvm_ir * ident list =
    match lookup sym_tab id with
    | None -> failwith ("Error : Call of the undefined function " ^ id)
    | Some (VariableSymbol _) ->
        failwith ("Error : Var symbol " ^ id ^ " is not callable")
    | Some (FunctionSymbol { arguments = args2 }) ->
        if verify_fun_call_compatibility T_Void T_Void args args2 sym_tab then
            let ir, args_struct, called = compute_args id args sym_tab [] in
            (ir @: llvm_call id args_struct, id :: called)
        else
            failwith
                ("Error : Incompatible arguments type and/or number in declaration \
                    and call of function " ^ id)

and compute_args (fun_id : ident) (args : expression list)
        (sym_tab : symbol_table) (called_func : ident list) :
        llvm_ir * ident * ident list =
    let rec aux (struct_id : ident) (args : expression list)
            (sym_tab : symbol_table) (called_func : ident list) (index : int) :
            llvm_ir * ident list =
        match args with
        | [] -> (empty_ir, called_func)
        | Unit1 (Unit0 (VarExpression id)) :: tl -> (
            match uniq_id_of_symbol_table sym_tab id with
            | Some (typ, un_id) when typ != Type_Int && typ != Type_Tid ->
                let arg_ptr = newtmp () in
                let arg_ptr_instr =
                    llvm_arg_ptr_in_struct arg_ptr fun_id struct_id index
                in
                let ptr_instr, out = instr_to_get_ptr id (LLVM_i32 0) sym_tab in
                let store_arg =
                    llvm_store_arg_in_struct (LLVM_var out) arg_ptr LLVM_type_i32_ptr
                in
                let ir2, called =
                    aux struct_id tl sym_tab called_func (index + 1)
                in
                ( (((empty_ir @: arg_ptr_instr) @: ptr_instr) @: store_arg) @@ ir2,
                    called )
            | _ ->
                let arg_ptr = newtmp () in
                let arg_ptr_instr =
                    llvm_arg_ptr_in_struct arg_ptr fun_id struct_id index
                in
                let ir, arg_value, called =
                    ir_of_expression (Unit1 (Unit0 (VarExpression id))) sym_tab
                in
                let store_arg =
                    llvm_store_arg_in_struct arg_value arg_ptr LLVM_type_i32
                in
                let ir2, called =
                    aux struct_id tl sym_tab called_func (index + 1)
                in
                ((((empty_ir @: arg_ptr_instr) @@ ir) @: store_arg) @@ ir2, called))
        | hd :: tl ->
            let arg_ptr = newtmp () in
            let arg_ptr_instr =
                llvm_arg_ptr_in_struct arg_ptr fun_id struct_id index
            in
            let ir, arg_value, called = ir_of_expression hd sym_tab in
            let store_arg =
                llvm_store_arg_in_struct arg_value arg_ptr LLVM_type_i32
            in
            let ir2, called = aux struct_id tl sym_tab called_func (index + 1) in
            ((((empty_ir @: arg_ptr_instr) @@ ir) @: store_arg) @@ ir2, called)
    in
    let tmp0 = newtmp () in
    let tmp1 = newtmp () in
    let args_storage, called = aux tmp0 args sym_tab called_func 0 in
    ( (empty_ir @: llvm_args_init tmp0 fun_id)
        @@ args_storage
        @: llvm_convert_args_ptr tmp1 fun_id tmp0,
        tmp1,
        called )

and ir_of_map_red tab cut_count tab_size fun_id args sym_tab :
        ident list * llvm_ir * ident list =
    let rec aux_map_red tab tab_size start map_size fun_id arg_values tids ir :
            ident list * llvm_ir =
        match tab_size - start with
        | n when n < 2 * map_size ->
            let tid = newtmp () in
            let ir2 =
                llvm_map_red tid start (tab_size - start) tab tab_size fun_id
                    arg_values
            in
            (tid :: tids, ir @@ ir2)
        | _ ->
            let tid = newtmp () in
            let ir2 =
                llvm_map_red tid start map_size tab tab_size fun_id arg_values
            in
            aux_map_red tab tab_size (start + map_size) map_size fun_id arg_values
                (tid :: tids) (ir @@ ir2)
    and aux_args ir args values called sym_tab :
            llvm_ir * ident list * (llvm_type * llvm_value) list =
        match args with
        | [] -> (empty_ir, called, values)
        | Unit1 (Unit0 (VarExpression id)) :: tl -> (
            match uniq_id_of_symbol_table sym_tab id with
            | Some (typ, uniq_id) when typ != Type_Int && typ != Type_Tid ->
                let ptr_instr, out = instr_to_get_ptr id (LLVM_i32 0) sym_tab in
                aux_args (ir @: ptr_instr) tl
                    ((LLVM_type_i32_ptr, LLVM_var out) :: values)
                    called sym_tab
            | Some _ ->
                let ir2, value, called2 =
                    ir_of_expression (Unit1 (Unit0 (VarExpression id))) sym_tab
                in
                aux_args (ir @@ ir2) tl
                    ((LLVM_type_i32, value) :: values)
                    (called @ called2) sym_tab
            | None -> failwith ("Error : unknown var " ^ id))
        | hd :: tl ->
            let ir2, value, called2 = ir_of_expression hd sym_tab in
            aux_args (ir @@ ir2) tl
                ((LLVM_type_i32, value) :: values)
                (called @ called2) sym_tab
    in
    match uniq_id_of_symbol_table sym_tab tab with
    | Some (Type_Int, _) ->
        failwith
            ("Error : first argument of MAP must be of type INT ARRAY, but " ^ tab
            ^ " is of type INT")
    | Some (Type_Tid, _) ->
        failwith
            ("Error : first argument of MAP must be of type INT ARRAY, but " ^ tab
            ^ " is of type TID")
    | None -> failwith ("Error : undefined var " ^ tab)
    | Some (_, uniqid) -> (
        match lookup sym_tab fun_id with
        | Some (FunctionSymbol { arguments = Tab_params tab :: Var_params size :: tl } ) ->
            let ir, called2, arg_values = aux_args empty_ir args [] [] sym_tab in
            let tids, ir2 =
                aux_map_red uniqid tab_size 0 (tab_size / cut_count) fun_id
                    arg_values [] empty_ir
            in
            (tids, ir @@ ir2, called2)
        | Some (FunctionSymbol _) ->
            failwith
                "Error : first and second arguments of a function used in a MAP \
                must be an INT array and an INT"
        | Some (VariableSymbol _) ->
            failwith ("Error : function symbol " ^ fun_id ^ " used as var")
        | None -> failwith ("Error : unknown function symbol " ^ fun_id))

and ir_of_join_map_red (tids : ident list) : llvm_ir =
    match tids with
    | [] -> empty_ir
    | tid :: tl -> ir_of_join_map_red tl @: llvm_join_void tid

and aux_declaration (var : decl_variable) (sym_tab : symbol_table) :
        llvm_instr * symbol_table =
    match var with
    | DVar id ->
        if lookup sym_tab id != None then
            failwith ("Error : multiple déclaration of " ^ id)
        else
            let un_id = newuniqid id in
            let ir = llvm_declar_var_int ~res_var:un_id ~res_type:LLVM_type_i32 in
            (ir, VariableSymbol (Type_Int, id, un_id) :: sym_tab)
    | DTab (id, size) ->
        if lookup sym_tab id != None then
            failwith ("Error : multiple déclaration of " ^ id)
        else
            let un_id = newuniqid id in
            let ir =
                llvm_declar_var_tab ~res_tab:un_id ~res_size:size
                    ~res_type:LLVM_type_i32
            in
            (ir, VariableSymbol (Type_Tab size, id, un_id) :: sym_tab)

and aux_declaration_tid (var : decl_variable) (sym_tab : symbol_table) :
        llvm_instr * symbol_table =
    match var with
    | DVar id ->
        if lookup sym_tab id != None then
            failwith ("Error : multiple déclaration of " ^ id)
        else
            let un_id = newuniqid id in
            let ir = llvm_declar_var_int ~res_var:un_id ~res_type:LLVM_type_i64 in
            (ir, VariableSymbol (Type_Tid, id, un_id) :: sym_tab)
    | DTab (id, _) -> failwith "Error : TID arrays are not supported yet"

(* all expressions have type LLVM_type_i32 *)
(* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression (e : expression) (sym_tab : symbol_table) :
        llvm_ir * llvm_value * ident list =
    match e with
    | AddExpression [] -> (empty_ir, LLVM_i32 0, [])
    | AddExpression (e1 :: e2) ->
        let ir1, v1, called = ir_of_exp_prio_1 e1 sym_tab in
        let ir2, v2, called2 = ir_of_expression (AddExpression e2) sym_tab in
        let x = newtmp () in
        let ir =
            ir1 @@ ir2
            @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2
        in
        (ir, LLVM_var x, called2 @ called)
    | MinusExpression [] -> (empty_ir, LLVM_i32 0, [])
    | MinusExpression (e1 :: e2) ->
        let ir1, v1, called = ir_of_exp_prio_1 e1 sym_tab in
        let ir2, v2, called2 = ir_of_expression (MinusExpression e2) sym_tab in
        let x = newtmp () in
        let ir =
            ir1 @@ ir2
            @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2
        in
        (ir, LLVM_var x, called @ called2)
    | Unit1 e -> ir_of_exp_prio_1 e sym_tab

and ir_of_exp_prio_1 (e : expPrio1) (sym_tab : symbol_table) :
        llvm_ir * llvm_value * ident list =
    match e with
    | MulExpression [] -> (empty_ir, LLVM_i32 1, [])
    | MulExpression (e1 :: e2) ->
        let ir1, v1, called = ir_of_exp_prio_0 e1 sym_tab in
        let ir2, v2, called2 = ir_of_exp_prio_1 (MulExpression e2) sym_tab in
        let x = newtmp () in
        let ir =
            ir1 @@ ir2
            @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2
        in
        (ir, LLVM_var x, called @ called2)
    | DivExpression [] -> (empty_ir, LLVM_i32 1, [])
    | DivExpression (e1 :: e2) ->
        let ir1, v1, called = ir_of_exp_prio_0 e1 sym_tab in
        let ir2, v2, called2 = ir_of_exp_prio_1 (DivExpression e2) sym_tab in
        let x = newtmp () in
        let ir =
            ir1 @@ ir2
            @: llvm_udiv ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2
        in
        (ir, LLVM_var x, called @ called2)
    | Unit0 e -> ir_of_exp_prio_0 e sym_tab

and ir_of_exp_prio_0 (e : expPrio0) (sym_tab : symbol_table) :
        llvm_ir * llvm_value * ident list =
    match e with
    | IntegerExpression i -> (empty_ir, LLVM_i32 i, [])
    | ParentheseExpression e -> ir_of_expression e sym_tab
    | CallFun (id, params) -> ir_of_call_expr id params sym_tab []
    | TabExpression (id, e) ->
        let ir, index, called = ir_of_expression e sym_tab in
        let out = newtmp () in
        let instr1, ptr = instr_to_get_ptr id index sym_tab in
        let instr2 = llvm_var_expr out ptr in
        ((ir @: instr1) @: instr2, LLVM_var out, called)
    | VarExpression e -> (
        match uniq_id_of_symbol_table sym_tab e with
        | None -> failwith ("Error : unknown var symbol " ^ e)
        | Some (Type_Tab _, _) ->
            failwith ("Error : tab symbol " ^ e ^ " used as var symbol")
        | Some (Type_Tab_Ptr, _) ->
            failwith ("Error : tab symbol " ^ e ^ " used as var symbol")
        | Some (typ, uniq_id) ->
            let out = newtmp () in
            (empty_ir @: llvm_var_expr out uniq_id, LLVM_var out, []))

and ir_of_call_expr (id : ident) (args : expression list)
        (sym_tab : symbol_table) (called_func : ident list) :
        llvm_ir * llvm_value * ident list =
    match lookup sym_tab id with
    | None -> failwith ("Error : Call of the undefined function " ^ id)
    | Some (VariableSymbol _) ->
        failwith ("Error : Var symbol " ^ id ^ " is not callable")
    | Some (FunctionSymbol { return_type = T_Void }) ->
        failwith
        ("Error : Void type function (" ^ id
            ^ ") cannot be called in an expression")
    | Some (FunctionSymbol { arguments = args2 }) ->
        if verify_fun_call_compatibility T_Int T_Int args args2 sym_tab then
            let ir, arg_struct, called = compute_args id args sym_tab called_func in
            let fun_dest = newtmp () in
            ( ir @: llvm_call_expr fun_dest id arg_struct,
                LLVM_var fun_dest,
                id :: called )
        else
            failwith
                ("Error : Incompatible arguments type and/or number in declaration \
                    and call of function " ^ id)

and verify_fun_call_compatibility (ret : ret_type) (ret2 : ret_type)
        (args : expression list) (args2 : params list) (sym_tab : symbol_table) :
        bool =
    let rec check_args (args : expression list) (args2 : params list) :
            bool =
        match (args, args2) with
        | [], [] -> true
        | [], _ -> false
        | _, [] -> false
        | Unit1 (Unit0 (VarExpression id)) :: tl1, Tab_params _ :: tl2 -> (
            match lookup sym_tab id with
            | Some (VariableSymbol (Type_Tab _, _, _)) -> true && check_args tl1 tl2
            | Some (VariableSymbol (Type_Tab_Ptr, _, _)) ->
                true && check_args tl1 tl2
            | _ -> false)
        | Unit1 (Unit0 (VarExpression id)) :: tl1, Var_params _ :: tl2 -> (
            match lookup sym_tab id with
            | Some (VariableSymbol (Type_Int, _, _)) -> true && check_args tl1 tl2
            | _ -> false)
        | _ :: tl1, Var_params _ :: tl2 -> true && check_args tl1 tl2
        | _ -> false
    in
    ret == ret2 && check_args args args2

and instr_to_get_ptr (tab : ident) (index : llvm_value) (sym_tab : symbol_table) :
        llvm_instr * llvm_var =
    match uniq_id_of_symbol_table sym_tab tab with
    | None -> failwith ("Error : unknown tab symbol " ^ tab)
    | Some (Type_Int, uniq_id) ->
        failwith ("Error : var symbol " ^ tab ^ " used as tab symbol")
    | Some (Type_Tid, uniq_id) ->
        failwith ("Error : TID symbol " ^ tab ^ " used as tab symbol")
    | Some (Type_Tab size, uniq_id) ->
        let ptr = newtmp () in
        (llvm_tab_expr ptr uniq_id index size, ptr)
    | Some (Type_Tab_Ptr, uniq_id) ->
        let ptr = newtmp () in
        (llvm_ptr_expr ptr uniq_id index, ptr)
