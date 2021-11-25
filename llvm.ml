open Utils

(* TODO : extend when you extend the language *)

(* This file contains a simple LLVM IR representation *)
(* and methods to generate its string representation  *)

type llvm_type =
  | LLVM_type_i32
  | LLVM_type_i32_ptr
  | LLVM_type_void
(* TODO: to complete *)

type llvm_var = string

type llvm_label = string
    
type llvm_value =
  | LLVM_i32 of int
  | LLVM_var of llvm_var
(* TODO: to complete? *)

			 
type llvm_ir = (* type of generated IR *)
  { header: llvm_instr_seq; (* instructions to be placed before all code (global definitions) *)
    body: llvm_instr_seq;
  }
    
 and llvm_instr_seq = (* type of sequences of instructions *)
   | Empty
   | Atom of llvm_instr
   | Concat of llvm_instr_seq * llvm_instr_seq

 and llvm_instr = string (* type of instructions *)

(* empty IR *)
let empty_ir = {
  header = Empty;
  body = Empty;
}

let glob_read = ref (newglob "read")

(* appending an instruction in the header: ir @^ i *)
let (@^) ir i = {
    header = Concat (ir.header, Atom i);
    body = ir.body;
  }
		 
(* appending an instruction in the body: ir @: i *)
let (@:) ir i = {
    header = ir.header;
    body = Concat (ir.body, Atom i);
  }
		 
(* concatenation of two IRs: ir1 @@ ir2 *)
let (@@) ir1 ir2 = {
    header = Concat (ir1.header, ir2.header);
    body = Concat (ir1.body, ir2.body);
}
		 
(* actual IR generation *)
let rec string_of_type = function
  | LLVM_type_i32 -> "i32"
  | LLVM_type_i32_ptr -> "i32*"
  | LLVM_type_void -> "void"

and string_of_var x = "%" ^ x
and string_of_fun_name x = "@" ^ x

and string_of_value = function
  | LLVM_i32 n -> string_of_int n
  | LLVM_var x -> string_of_var x
		     
and string_of_ir ir =
  (* this header describe to LLVM the target
   * and declare the external function printf
   *)
  "; Target\n"
  ^ "target triple = \"x86_64-unknown-linux-gnu\"\n"
  ^ "; External declaration of the printf function\n"
  ^ "declare i32 @printf(i8* noalias nocapture, ...)\n"
  ^ "declare i32 @scanf(i8* noalias nocapture, ...)\n"
  ^ "\n; Actual code begins\n\n"
  ^ !glob_read ^ " = global [3 x i8] c\"%d\\00\"\n"
  ^ string_of_instr_seq ir.header
  ^ "\n"
  ^ string_of_instr_seq ir.body

and string_of_instr_seq = function
  | Empty -> ""
  | Atom i -> i
  | Concat (li1,li2) -> string_of_instr_seq li1 ^ string_of_instr_seq li2

and string_of_print_args args =
  match args with
  | [] -> ""
  | a::q -> ", i32 " ^ string_of_value a ^ string_of_print_args q

and string_of_args args =
  match args with
  | [] -> ""
  | [id] -> "i32 " ^  string_of_value id 
  | id::tl -> "i32 " ^ string_of_value id ^ ", " ^ string_of_args tl

and string_of_header_args args =
  match args with
  | [] -> ""
  | [var_type, id] -> string_of_type var_type ^ " " ^ string_of_var id 
  | (var_type, id)::tl -> string_of_type var_type ^ " " ^ string_of_var id ^ ", " ^ string_of_header_args tl

and string_of_instr i = i

			  
(* functions for the creation of various instructions *)

let llvm_add ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = add " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_mul ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr = 
  string_of_var res_var ^ " = mul " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_sub ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr = 
  string_of_var res_var ^ " = sub " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_udiv ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr = 
  string_of_var res_var ^ " = udiv " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_return ~(ret_type : llvm_type) ~(ret_value : llvm_value) : llvm_instr =
  "ret " ^ string_of_type ret_type ^ " " ^ string_of_value ret_value ^ "\n"

let llvm_declar_var_int ~(res_var : llvm_var) ~(res_type : llvm_type) : llvm_instr = 
  string_of_var res_var ^ " = alloca " ^ string_of_type res_type ^ "\n" 

let llvm_declar_var_tab ~(res_tab : llvm_var) ~(res_size : llvm_value) ~(res_type : llvm_type) : llvm_instr = 
  string_of_var res_tab ^ " = alloca [" ^ string_of_value res_size ^ " x " ^ string_of_type res_type ^ " ]\n"
let llvm_var_expr ~(dest : llvm_var) ~(var : llvm_var) : llvm_instr =
  string_of_var dest ^ " = load i32, i32* " ^ string_of_var var ^ "\n"
let llvm_affect_var ~(res_var : llvm_value) ~(val_var : llvm_var) : llvm_instr =
  "store i32 " ^ string_of_value res_var ^ ", i32* " ^ string_of_var val_var ^ "\n"
  (* defining the 'main' function with ir.body as function body *)
let llvm_print ~(print_str : llvm_var) ~(len : int)~(print_args : llvm_value list) : llvm_instr =
  "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([" ^ string_of_int len ^ " x i8], [" ^ string_of_int len ^ " x i8]* " ^ print_str ^ ", i64 0, i64 0)" ^ string_of_print_args print_args ^ " )\n"
let llvm_str ~(str_label : llvm_label) ~(size : int) ~(str : string) : llvm_instr =
  str_label ^ " = global [ " ^ string_of_int size ^ " x i8 ] c\"" ^ str ^ "\"\n"
let llvm_read ~(read_var : llvm_var) : llvm_instr =
  "call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* " ^ !glob_read ^ ", i64 0, i64 0), i32* " ^ string_of_var read_var ^ " )\n"
let llvm_if ~(if_cond : llvm_value) ~(jump_if : llvm_label) ~(jump_else : llvm_label) : llvm_instr =
  "br i1 " ^ string_of_value if_cond ^ ", label %" ^ jump_if ^ ", label %" ^ jump_else ^ "\n"
let llvm_cmp ~(out : llvm_var) ~(cond : llvm_value) : llvm_instr =
  string_of_var out ^ " = icmp ne i32 " ^ string_of_value cond ^ ", 0\n"
let llvm_jump ~(jump_label : llvm_label) : llvm_instr =
  "br label %" ^ jump_label ^ "\n"
let llvm_ret ~(ret_val : llvm_value) : llvm_instr =
  "ret i32 " ^ string_of_value ret_val ^ "\n"
let llvm_call ~(id : llvm_var) ~(args : llvm_value list) : llvm_instr =
  "call void " ^ string_of_fun_name id ^  "(" ^ string_of_args args ^ ") noreturn\n"
let llvm_call_expr ~(out : llvm_var) ~(id : llvm_var) ~(args : llvm_value list) : llvm_instr =
  "%" ^ out ^ " = call i32 " ^ string_of_fun_name id ^  "(" ^ string_of_args args ^ ")\n"
let llvm_fun_header ~(ret_type : llvm_type) ~(id : llvm_var) ~(args : (llvm_type * llvm_var) list) : llvm_instr =
  "define " ^ string_of_type ret_type ^ " " ^ string_of_fun_name id ^ "(" ^ string_of_header_args args ^ ") {\n"
let llvm_label ~(label : llvm_label) : llvm_instr =
  label ^ ":\n"

let llvm_define_main (ir : llvm_ir) : llvm_ir =
  { header = ir.header;
    body = Atom ("define i32 @main() {\n" ^ string_of_instr_seq ir.body ^ "}\n");
  }
									 
(* TODO: complete with other LLVM instructions *)
								
