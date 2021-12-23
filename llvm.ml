open Utils

(* TODO : extend when you extend the language *)

(* This file contains a simple LLVM IR representation *)
(* and methods to generate its string representation    *)

type llvm_type =
    | LLVM_type_i32
    | LLVM_type_i64
    | LLVM_type_i32_ptr
    | LLVM_type_void
(* TODO: to complete *)

type llvm_var = string

type llvm_label = string

type llvm_value = LLVM_i32 of int | LLVM_var of llvm_var
(* TODO: to complete? *)

type llvm_ir =
    { (* type of generated IR *)
        header: llvm_instr_seq
    ; (* instructions to be placed before all code (global definitions) *)
        body: llvm_instr_seq }

and llvm_instr_seq =
    (* type of sequences of instructions *)
    | Empty
    | Atom of llvm_instr
    | Concat of llvm_instr_seq * llvm_instr_seq

and llvm_instr = string
(* type of instructions *)

(* empty IR *)
let empty_ir = {header= Empty; body= Empty}

let glob_read = ref (newglob "read")

(* appending an instruction in the header: ir @^ i *)
let ( @^ ) ir i = {header= Concat (ir.header, Atom i); body= ir.body}

(* appending an instruction in the body: ir @: i *)
let ( @: ) ir i = {header= ir.header; body= Concat (ir.body, Atom i)}

(* concatenation of two IRs: ir1 @@ ir2 *)
let ( @@ ) ir1 ir2 =
    {header= Concat (ir1.header, ir2.header); body= Concat (ir1.body, ir2.body)}

(* actual IR generation *)
let string_of_type = function
    | LLVM_type_i32 ->
            "i32"
    | LLVM_type_i64 ->
            "i64"
    | LLVM_type_i32_ptr ->
            "i32*"
    | LLVM_type_void ->
            "void"

let string_of_var x = "%" ^ x

let string_of_fun_name x = "@" ^ x

let string_of_struct_id x = "%" ^ x ^ "_args"

let string_of_value = function
    | LLVM_i32 n ->
            string_of_int n
    | LLVM_var x ->
            string_of_var x

let rec string_of_ir ir =
    (* this header describe to LLVM the target
     * and declare the external function printf
     *)
    "; Target\n" ^ "target triple = \"x86_64-pc-linux-gnu\"\n"
    ^ "; External declaration of the printf function\n"
    ^ "declare i32 @printf(i8* noalias nocapture, ...)\n"
    ^ "declare i32 @scanf(i8* noalias nocapture, ...)\n\n"
    ^ "%union.pthread_attr_t = type { i64, [48 x i8] }\n\n"
    ^ "declare i32 @pthread_create(i64*, %union.pthread_attr_t*, i8* (i8*)*, \
         i8*)\n\n" ^ "declare i32 @pthread_join(i64, i8**)\n"
    ^ "\n; Actual code begins\n\n" ^ !glob_read
    ^ " = global [3 x i8] c\"%d\\00\"\n"
    ^ string_of_instr_seq ir.header
    ^ "\n"
    ^ string_of_instr_seq ir.body

and string_of_instr_seq = function
    | Empty ->
            ""
    | Atom i ->
            i
    | Concat (li1, li2) ->
            string_of_instr_seq li1 ^ string_of_instr_seq li2

and string_of_print_args args =
    match args with
    | [] ->
            ""
    | a :: q ->
            ", i32 " ^ string_of_value a ^ string_of_print_args q

and string_of_args args =
    match args with
    | [] ->
            ""
    | [(typ, id)] ->
            string_of_type typ ^ " " ^ string_of_value id
    | (typ, id) :: tl ->
            string_of_type typ ^ " " ^ string_of_value id ^ ", " ^ string_of_args tl

and string_of_args_bis args =
    match args with
    | [] ->
            ""
    | [typ] ->
            string_of_type typ
    | typ :: tl ->
            string_of_type typ ^ ", " ^ string_of_args_bis tl

and string_of_header_args args =
    match args with
    | [] ->
            ""
    | [(var_type, id)] ->
            string_of_type var_type ^ " " ^ string_of_var id
    | (var_type, id) :: tl ->
            string_of_type var_type ^ " " ^ string_of_var id ^ ", "
            ^ string_of_header_args tl

and string_of_instr i = i

(* functions for the creation of various instructions *)

(* DECLARATIONS *)

let llvm_declar_var_int ~(res_var : llvm_var) ~(res_type : llvm_type) :
        llvm_instr =
    string_of_var res_var ^ " = alloca " ^ string_of_type res_type ^ "\n"

let llvm_declar_var_tab
        ~(res_tab : llvm_var)
        ~(res_size : int)
        ~(res_type : llvm_type) : llvm_instr =
    string_of_var res_tab ^ " = alloca [" ^ string_of_int res_size ^ " x "
    ^ string_of_type res_type ^ " ]\n"

let llvm_str ~(str_label : llvm_label) ~(size : int) ~(str : string) :
        llvm_instr =
    str_label ^ " = global [ " ^ string_of_int size ^ " x i8 ] c\"" ^ str ^ "\"\n"

(* AFFECTATIONS *)

let llvm_affect_var ~(res_var : llvm_value) ~(val_var : llvm_var) : llvm_instr =
    "store i32 " ^ string_of_value res_var ^ ", i32* " ^ string_of_var val_var
    ^ "\n"

(* EXPRESSIONS *)

let llvm_add
        ~(res_var : llvm_var)
        ~(res_type : llvm_type)
        ~(left : llvm_value)
        ~(right : llvm_value) : llvm_instr =
    string_of_var res_var ^ " = add " ^ string_of_type res_type ^ " "
    ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_mul
        ~(res_var : llvm_var)
        ~(res_type : llvm_type)
        ~(left : llvm_value)
        ~(right : llvm_value) : llvm_instr =
    string_of_var res_var ^ " = mul " ^ string_of_type res_type ^ " "
    ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_sub
        ~(res_var : llvm_var)
        ~(res_type : llvm_type)
        ~(left : llvm_value)
        ~(right : llvm_value) : llvm_instr =
    string_of_var res_var ^ " = sub " ^ string_of_type res_type ^ " "
    ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_udiv
        ~(res_var : llvm_var)
        ~(res_type : llvm_type)
        ~(left : llvm_value)
        ~(right : llvm_value) : llvm_instr =
    string_of_var res_var ^ " = udiv " ^ string_of_type res_type ^ " "
    ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_var_expr ~(dest : llvm_var) ~(var : llvm_var) : llvm_instr =
    string_of_var dest ^ " = load i32, i32* " ^ string_of_var var ^ "\n"

let llvm_tab_expr
        ~(dest : llvm_var)
        ~(tab : llvm_var)
        ~(index : llvm_value)
        ~(size : int) : llvm_instr =
    string_of_var dest ^ " = getelementptr [" ^ string_of_int size ^ " x i32], ["
    ^ string_of_int size ^ " x i32]* " ^ string_of_var tab ^ ", i64 0, i32 "
    ^ string_of_value index ^ "\n"

let llvm_ptr_expr ~(dest : llvm_var) ~(tab : llvm_var) ~(index : llvm_value) :
        llvm_instr =
    string_of_var dest ^ " = getelementptr i32, i32* " ^ string_of_var tab
    ^ ", i32 " ^ string_of_value index ^ "\n"

let llvm_call_expr ~(out : llvm_var) ~(id : llvm_var) ~(args : llvm_var) :
        llvm_instr =
    let tmp0 = newtmp () in
    string_of_var tmp0 ^ " = call i8* " ^ string_of_fun_name id ^ "(i8* "
    ^ string_of_var args ^ ")\n" ^ string_of_var out ^ " = ptrtoint i8* "
    ^ string_of_var tmp0 ^ " to i32\n"

(* INSTRUCTIONS *)

(* PRINT *)
let llvm_print
        ~(print_str : llvm_var)
        ~(len : int)
        ~(print_args : llvm_value list) : llvm_instr =
    "call i32 (i8*, ...) @printf(i8* getelementptr inbounds (["
    ^ string_of_int len ^ " x i8], [" ^ string_of_int len ^ " x i8]* " ^ print_str
    ^ ", i64 0, i64 0)"
    ^ string_of_print_args print_args
    ^ " )\n"

(* READ *)
let llvm_read ~(read_var : llvm_var) : llvm_instr =
    "call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* "
    ^ !glob_read ^ ", i64 0, i64 0), i32* " ^ string_of_var read_var ^ " )\n"

(* IF / WHILE *)
let llvm_if
        ~(if_cond : llvm_value)
        ~(jump_if : llvm_label)
        ~(jump_else : llvm_label) : llvm_instr =
    "br i1 " ^ string_of_value if_cond ^ ", label %" ^ jump_if ^ ", label %"
    ^ jump_else ^ "\n"

let llvm_cmp ~(out : llvm_var) ~(cond : llvm_value) : llvm_instr =
    string_of_var out ^ " = icmp ne i32 " ^ string_of_value cond ^ ", 0\n"

let llvm_jump ~(jump_label : llvm_label) : llvm_instr =
    "br label %" ^ jump_label ^ "\n"

let llvm_label ~(label : llvm_label) : llvm_instr = label ^ ":\n"

(* CALL *)
let llvm_args_init ~(out : llvm_var) ~(fun_id : llvm_var) : llvm_instr =
    string_of_var out ^ " = alloca " ^ string_of_struct_id fun_id ^ "\n"

let llvm_call ~(id : llvm_var) ~(arg_struct : llvm_var) : llvm_instr =
    let tmp0 = newtmp () in
    string_of_var tmp0 ^ " = call i8* " ^ string_of_fun_name id ^ "(i8* "
    ^ string_of_var arg_struct ^ ")\n"

let llvm_arg_ptr_in_struct
        ~(arg_ptr : llvm_var)
        ~(fun_id : llvm_var)
        ~(struct_ptr : llvm_var)
        ~(index : int) : llvm_instr =
    string_of_var arg_ptr ^ " = getelementptr " ^ string_of_struct_id fun_id
    ^ ", " ^ string_of_struct_id fun_id ^ "* " ^ string_of_var struct_ptr
    ^ ", i32 0, i32 " ^ string_of_int index ^ "\n"

let llvm_store_arg_in_struct
        ~(out : llvm_value)
        ~(arg_ptr : llvm_var)
        ~(typ : llvm_type) : llvm_instr =
    "store " ^ string_of_type typ ^ " " ^ string_of_value out ^ ", "
    ^ string_of_type typ ^ "* " ^ string_of_var arg_ptr ^ "\n"

let llvm_convert_args_ptr
        ~(out : llvm_var)
        ~(fun_id : llvm_var)
        ~(args : llvm_var) =
    string_of_var out ^ " = bitcast " ^ string_of_struct_id fun_id ^ "* "
    ^ string_of_var args ^ " to i8*\n"

(* RETURN *)
let llvm_ret ~(ret_val : llvm_value) : llvm_instr =
    let tmp0 = newtmp () in
    string_of_var tmp0 ^ " = inttoptr i32 " ^ string_of_value ret_val
    ^ " to i8*\n" ^ "ret i8* " ^ string_of_var tmp0 ^ "\n"

(* THREAD *)
let llvm_create_thread ~(tid : llvm_var) ~(fun_id : llvm_var) ~(args : llvm_var)
        : llvm_instr =
    "call i32 @pthread_create(i64* " ^ string_of_var tid
    ^ ", %union.pthread_attr_t* null, i8* (i8*)* " ^ string_of_fun_name fun_id
    ^ ", i8* " ^ string_of_var args ^ ")\n"

(* JOIN *)
let llvm_join_void ~(tid : llvm_var) : llvm_instr =
    let tid_tmp = newtmp () in
    string_of_var tid_tmp ^ " = load i64, i64* " ^ string_of_var tid ^ "\n"
    ^ "call i32 @pthread_join(i64 " ^ string_of_var tid_tmp ^ ", i8** null)\n"

let llvm_join_int ~(tid : llvm_var) ~(out : llvm_var) : llvm_instr =
    let tid_tmp = newtmp () in
    let tmp0 = newtmp () in
    let tmp1 = newtmp () in
    let tmp2 = newtmp () in
    string_of_var tid_tmp ^ " = load i64, i64* " ^ string_of_var tid ^ "\n"
    ^ string_of_var tmp0 ^ " = alloca i8*\n" ^ "call i32 @pthread_join(i64 "
    ^ string_of_var tid_tmp ^ ", i8** " ^ string_of_var tmp0 ^ ")\n"
    ^ string_of_var tmp1 ^ " = load i8*, i8** " ^ string_of_var tmp0 ^ "\n"
    ^ string_of_var tmp2 ^ " = ptrtoint i8* " ^ string_of_var tmp1 ^ " to i32\n"
    ^ "store i32 " ^ string_of_var tmp2 ^ ", i32* " ^ string_of_var out ^ "\n"

(* MAP *)
let rec llvm_map_red
        (tid : llvm_var)
        (start : int)
        (map_size : int)
        (tab : llvm_var)
        (tab_size : int)
        (fun_id : llvm_var)
        (args : (llvm_type * llvm_value) list) : llvm_ir =
    let tmp0 = newtmp () in
    let tmp1 = newtmp () in
    let tmp2 = newtmp () in
    let tmp3 = newtmp () in
    let tmp4 = newtmp () in
    empty_ir @: string_of_var tmp0 ^ " = alloca " ^ string_of_struct_id fun_id
    ^ "\n" ^ string_of_var tmp1 ^ " = getelementptr " ^ string_of_struct_id fun_id
    ^ ", " ^ string_of_struct_id fun_id ^ "* " ^ string_of_var tmp0
    ^ ", i32 0, i32 0\n" ^ string_of_var tmp2 ^ " = getelementptr "
    ^ string_of_struct_id fun_id ^ ", " ^ string_of_struct_id fun_id ^ "* "
    ^ string_of_var tmp0 ^ ", i32 0, i32 1\n" ^ string_of_var tmp3
    ^ " = getelementptr [" ^ string_of_int tab_size ^ " x i32], ["
    ^ string_of_int tab_size ^ " x i32]* " ^ string_of_var tab ^ ", i32 0, i32 "
    ^ string_of_int start ^ "\n" ^ "store i32* " ^ string_of_var tmp3 ^ ", i32** "
    ^ string_of_var tmp1 ^ "\n" ^ "store i32 " ^ string_of_int map_size
    ^ ", i32* " ^ string_of_var tmp2 ^ "\n"
    ^ llvm_store_args_map tmp0 fun_id 2 args
    ^ string_of_var tmp4 ^ " = bitcast " ^ string_of_struct_id fun_id ^ "* "
    ^ string_of_var tmp0 ^ " to i8*\n" ^ string_of_var tid ^ " = alloca i64\n"
    ^ "call i32 @pthread_create(i64* " ^ string_of_var tid
    ^ ", %union.pthread_attr_t* null, i8* (i8*)* " ^ string_of_fun_name fun_id
    ^ ", i8* " ^ string_of_var tmp4 ^ ")\n"

and llvm_store_args_map
        (struct_id : llvm_var)
        (fun_id : llvm_var)
        (index : int)
        (args : (llvm_type * llvm_value) list) : llvm_instr =
    match args with
    | [] ->
            ""
    | (typ, id) :: tl ->
            let tmp0 = newtmp () in
            string_of_var tmp0 ^ " = getelementptr " ^ string_of_struct_id fun_id
            ^ ", " ^ string_of_struct_id fun_id ^ "* " ^ string_of_var struct_id
            ^ ", i32 0, i32 " ^ string_of_int index ^ "\n" ^ "store "
            ^ string_of_type typ ^ " " ^ string_of_value id ^ ", "
            ^ string_of_type typ ^ "* " ^ string_of_var tmp0 ^ "\n"
            ^ llvm_store_args_map struct_id fun_id (index + 1) tl

(* FUNCTIONS *)

let llvm_struct_of_args ~(fun_id : llvm_var) ~(args : llvm_type list) :
        llvm_instr =
    string_of_struct_id fun_id ^ " = type {" ^ string_of_args_bis args ^ "}\n"

let llvm_fun_header ~(id : llvm_var) : llvm_instr =
    "define i8* " ^ string_of_fun_name id ^ "(i8* %_args) {\n"

let llvm_args_struct ~(out : llvm_var) ~(struct_id : llvm_var) : llvm_instr =
    string_of_var out ^ " = bitcast i8* %_args to "
    ^ string_of_struct_id struct_id
    ^ "*\n"

let llvm_extract_var_arg
        (id : llvm_var)
        (fun_id : llvm_var)
        (struct_id : llvm_var)
        (index : int) : llvm_instr =
    string_of_var id ^ " = getelementptr " ^ string_of_struct_id fun_id ^ ", "
    ^ string_of_struct_id fun_id ^ "* " ^ string_of_var struct_id
    ^ ", i32 0, i32 " ^ string_of_int index ^ "\n"

let llvm_extract_tab_arg
        (id : llvm_var)
        (fun_id : llvm_var)
        (struct_id : llvm_var)
        (index : int) : llvm_instr =
    let tmp0 = newtmp () in
    string_of_var tmp0 ^ " = getelementptr " ^ string_of_struct_id fun_id ^ ", "
    ^ string_of_struct_id fun_id ^ "* " ^ string_of_var struct_id
    ^ ", i32 0, i32 " ^ string_of_int index ^ "\n" ^ string_of_var id
    ^ " = load i32*, i32** " ^ string_of_var tmp0 ^ "\n"

let llvm_routine_header ~(id : llvm_var) ~(tab_id : llvm_var) ~(size : llvm_var)
        : llvm_instr =
    let tmp0 = newtmp () in
    let tmp1 = newtmp () in
    let tmp2 = newtmp () in
    "define i8* " ^ string_of_fun_name id ^ "(i8* %_args) {\n"
    ^ string_of_var tmp0 ^ " = bitcast i8* %_args to %routine_args*\n"
    ^ string_of_var tmp1 ^ " = getelementptr %routine_args, %routine_args* "
    ^ string_of_var tmp0 ^ ", i32 0, i32 0\n" ^ string_of_var tmp2
    ^ " = getelementptr %routine_args, %routine_args* " ^ string_of_var tmp0
    ^ ", i32 0, i32 1\n" ^ string_of_var tab_id ^ " = load i32*, i32** "
    ^ string_of_var tmp1 ^ "\n" ^ string_of_var size ^ " = load i32, i32* "
    ^ string_of_var tmp2 ^ "\n"
