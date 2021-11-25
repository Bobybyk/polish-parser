open Printf

(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

let eval_comp condition =
  let compare val1 comp_type val2 =
    match comp_type with
      | Eq -> if val1 = val2 then true else false
      | Ne -> if val1 = val2 then false else true
      | Lt -> if val1 < val2 then true else false
      | Le -> if val1 <= val2 then true else false
      | Gt -> if val1 > val1 then true else false
      | Ge -> if val1 >= val2 then true else false
    in match condition with (val1, comp_type, val2) -> compare val1 comp_type val2;;

let read_file (file:string) =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


(* let rec collect_term line = 
  match line with
    | [] -> Var("empty line")
    | _::[] -> Var("empty line")
    | e::e'::line' -> match e with
      | "+" -> Op(Add, (collect_term(e'::line')), (collect_term(line')))
      | "-" -> Op(Sub, (collect_term(e'::line')), (collect_term(line')))
      | "*" -> Op(Mul, (collect_term(e'::line')), (collect_term(line')))
      | "/" -> Op(Div, (collect_term(e'::line')), (collect_term(line')))
      | "%" -> Op(Mod, (collect_term(e'::line')), (collect_term(line')))
*)

let collect_name line =
  match line with 
    | [] -> failwith("empty line")
    | e::line' -> e;;

let is_int str =
  let verif_num n =
    try (int_of_string n |> string_of_int) = n
    with Failure _ -> false in 
  verif_num str;;

let rec collect_expr line =
  match line with
    | [] -> failwith("empty line")
    | e::e'::l -> (match e with
      | "+" -> Op(Add, collect_expr (e'::l), collect_expr l)
      | "-" -> Op(Sub, collect_expr (e'::l), collect_expr l)
      | "*" -> Op(Mul, collect_expr (e'::l), collect_expr l)
      | "/" -> Op(Div, collect_expr (e'::l), collect_expr l)
      | "%" -> Op(Mod, collect_expr (e'::l), collect_expr l)
      | _ -> if is_int e then Num(int_of_string e) else Var(e))
    | e::l -> if is_int e then Num(int_of_string e) else Var(e) 

let collect_instr line =
  let line_per_string = String.split_on_char ' ' line in
  match line_per_string with
    | [] -> failwith("empty line")
    | e::l -> (match e with
      | "READ" -> Read(collect_name l) 
      | "IF" -> failwith("TODO")
      | "WHILE" -> failwith("TODO")
      | "PRINT" -> Print(collect_expr l)
      | _ -> failwith("empty line"))
(***********************************************************************)

(* tests *)
let file_content = read_file "exemples/abs.p";;
let () = List.iter (printf "%s ") file_content;;

(***********************************************************************)

let read_polish (filename:string) : program = 
  let program = read_file filename in 
    let rec browse_program program acc = 
      match program with
      | [] -> []
      | e::program' -> (acc+1,collect_instr e)::browse_program program' (acc+1)
    in  browse_program program 0
  

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
