open List
open ASD
open Utils

(* This file contains the symbol table definition. *)
(* A symbol table contains a set of ident and the  *)
(* corresponding symbols.                          *)
(* The order is important: this first match count  *)

type function_symbol_state = Defined | Declared

type uniq_ident = string

type function_symbol = {
  return_type: ret_type;
  identifier: ident;
  arguments: params list;
  state: function_symbol_state;
}

and symbol =
  | VariableSymbol of typ * ident * uniq_ident
  | FunctionSymbol of function_symbol

and symbol_table = symbol list

(* Not necessarary anymore ? 
and variable_table = Vars of variable list 

let rec get_variable_value var_table id = match var_table with 
  |Vars([]) -> None 
  |Vars((Var(id0, v))::q) -> if id0 = id then match v with 
        | None -> None 
        | Some x -> Some x
      else  get_variable_value (Vars q) id 
  |Vars(Tab(_,_,_)::q) -> failwith "todo" 
*)

(* public interface *)
let lookup tab id =
  let rec assoc key = function (* like List.assoc, but with deep hidden keys *)
    | ((VariableSymbol (_, id, _)) as r) :: q
    | (FunctionSymbol {identifier = id; _} as r) :: q ->
        if key = id then
          Some r
        else
          assoc key q
    | [] -> None

  in assoc id tab

let uniq_id_of_symbol_table tab id =
  match lookup tab id with
  | None -> None
  | Some(FunctionSymbol(_)) -> None
  | Some(VariableSymbol(_, id, uniq_id)) -> Some uniq_id

let add tab sym = sym :: tab

(* Note : obviously not symmetric *)
let merge = (@)
