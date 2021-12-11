let tmp = ref 0
let lab = ref 0
let glob = ref 0
let uniq_ident = ref 0
let func = ref 0

(* generate a new unique local identifier (starting with %) *)
let newtmp: unit -> string = function () ->
  tmp := succ !tmp;
  "tmp_" ^ (string_of_int !tmp)

(* generate a new unique label starting with str *)
let newlab str =
  lab := succ !lab;
  str ^ "_" ^ (string_of_int !lab)

(* generate a new unique global identifier (starting with @str) *)
let newglob str =
  glob := succ !glob;
  "@" ^ str ^ "_" ^ (string_of_int !glob)

(* generate a new unique identifier. Used to differenciate multiple declaration of variable with same name. *)
let newuniqid str =
  incr uniq_ident;
  str ^ "_" ^ string_of_int !uniq_ident

let newfun str =
  match str with
  | "main" -> "@" ^ str
  | _ -> incr func; "@" ^ str ^ "_" ^ string_of_int !func

(* transform escaped newlines ('\' 'n') into newline form suitable for LLVM
 * and transform tabulations ('\' 't') into tabulations form suitable for LLVM
 * and append the NUL character (end of string)
 * return a pair: the new string, and its size (according to LLVM)
 *)
let string_transform str =
  let re_n = Str.regexp_string "\\n"
  (* replace all \n by \0A and append an \00 at the end.
   * return a pair: the new string and the number of matches
   *)
  in let re_t = Str.regexp_string "\\t"
  (* replace all \n by \09.
   * return a pair: the new string and the number of matches
   *)
  in let rec aux_n str pos matches =
    try
      let _ = Str.search_forward re_n str pos in (* can raise Not_found *)
      let str' = Str.replace_first re_n "\\\\0A" str
      in aux_n str' (1 + Str.match_beginning ()) (succ matches)
    with Not_found ->
      (str ^ "\\00", matches)
  in let rec aux_t str pos matches =
    try
      let _ = Str.search_forward re_t str pos in (* can raise Not_found *)
      let str' = Str.replace_first re_t "\\\\09" str
      in aux_t str' (1 + Str.match_beginning ()) (succ matches)
    with Not_found ->
      (str, matches)
  in let r = aux_n str 0 0
  in let r2 = aux_t (fst r) 0 (snd r)
  in (fst r2, 1 + (String.length str) - (snd r2))
  (*         + 1 for \00             - 1 by \n because each ('\' '\n') is transformed into one char *)
