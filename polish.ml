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

let read_file (file:string) : ((int * string) list) =
  let fic = open_in file in
    let ret = ref [(0,"")] in
    let nbr_line = ref 0 in 
    try 
      while true do
      nbr_line := (!nbr_line + 1);
        ret := (!nbr_line, (input_line fic))::!ret
      done; [] 
    with | End_of_file -> close_in_noerr (fic);
    List.rev(!ret);;

let read_file2 (file:string) =
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

(***********************************************************************)

(* tests *)
let file_content = read_file2 "exemples/abs.p";;
let file_content_with_line_nbr = read_file "exemples/abs.p";;
let () = List.iter (printf "%s ") file_content;;
(* let () = List.iter (printf "jpp je trouve pas les flags pour print") file_content_with_line_nbr;; *)

(***********************************************************************)

let read_polish (filename:string) : program = failwith "TODO"

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
