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

(* Un programme Polish est un bloc d'instructions *)
type program = block


(* Ici on évalue la valeur de vérité de deux valeurs en fonction d'un opérateur logique *)
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


(* Ici, via un chemin passé en argument, on récupère le contenu
 d'un fichier de type Polish dans une chaine de charactère. On va ensuite
 transférer le contenu de cette variable dans une liste de chaines de caractères 
 avec, pour chaque élément de cette liste, un "mot" identifié préalablement par 
 des caractères vides comme délimiteurs en début et fin
*)
let read_file (file:string) : (string list)=
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(* Fonction appelée si l'instruction en préfix est de type READ.
  On renvoie donc e la suite de la ligne à lire*)
let collect_name line : name =
  match line with 
    | [] -> failwith("empty line")
    | e::line' -> e;;

(* Fonction pour évaluer si l'argument est un entier *)
let is_int str : bool=
  let verif_num n =
    try (int_of_string n |> string_of_int) = n
    with Failure _ -> false in 
  verif_num str;;

(* Ici on parcourt la ligne pour identifier les opérateurs qu'on va ajouter
  dans une structure Op avec ses valeurs *)
let rec collect_expr line : expr=
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

(* Ici on va chercher à collecter le type d'instruction du début de la ligne 
  passée en argument. Pour chaque type d'instruction identifiée, on crée la bonne
  structure avec les opérateurs et expressions associées *)
let collect_instr line : instr=
  let line_per_string = String.split_on_char ' ' line in
  match line_per_string with
    | [] -> failwith("empty line")
    | e::l -> (match e with
      | "READ" -> Read(collect_name l) 
      | "IF" -> failwith("TODO")
      | "WHILE" -> failwith("TODO")
      | "PRINT" -> Print(collect_expr l)
      | _ -> failwith("empty line"))

let is_empty (ls: 'a list) : bool = List.length ls = 0;;

let rec reprint_polish (program:program) (ind_nbr:int) : unit= 
        let rec print_indentation ind =
                if ind > 0 then (printf "  " ; print_indentation (ind-1)) and
        print_expr (expr:expr) =
                (match expr with 
                | Num(i) -> printf "%d " i
                | Var(n) -> printf "%s " n
                | Op(o,e1,e2) -> print_op o ; print_expr e1 ; print_expr e2) and
        print_op (op:op) = 
                (match op with
                | Add -> printf "+ " 
                | Sub -> printf "- " 
                | Mul -> printf "* "
                | Div -> printf"/ " 
                | Mod -> printf "%% ") and 
        print_cond (cond:cond) = 
                (match cond with
                | (e1,c,e2) -> print_expr e1 ; print_comp c ; print_expr e2) and
        print_comp (comp:comp) =
                (match comp with
                | Eq -> printf "= "
                | Ne -> printf "<> "
                | Lt -> printf "< "
                | Le -> printf "<= "
                | Gt -> printf "> "
                | Ge -> printf ">= ") and
        print_block (block:block) ind_nbr = 
                printf "\n" ; print_indentation ind_nbr ;reprint_polish block ind_nbr and
        print_instr (instr:instr) ind_nbr =
              (match instr with
                | Set(n,e) -> (printf "%s := " n ) ; (print_expr e)
                | Read(n) -> printf "READ %s" n 
                | Print(e) -> printf "PRINT " ; print_expr e 
                | If(c,b,b2) -> printf "IF " ; print_cond c ; print_block b (ind_nbr+1);  if not(is_empty b2) then (printf "\nELSE " ; print_block b2 (ind_nbr+1))
                | While(c,b) -> printf "WHILE" ; print_cond c ; reprint_polish b ind_nbr ) in
        match program with
        | e::[] -> print_instr (snd e) ind_nbr
        | e::l -> print_instr (snd e) ind_nbr; printf "\n"  ; reprint_polish l ind_nbr
        | _ -> printf "";;

(***********************************************************************)

(* tests *)
(* let file_content = read_file "exemples/abs.p";; *)
(* let () = List.iter (printf "%s ") file_content;; *)

(* absolute value function *)
let condi = (Var("n"),Lt,Num(0));;
let block1 = [(3,Set("res",Op(Sub,Num(0),Var("n"))))];;
let block2 = [(5,Set("res",Var("n")))];;
let ifs = If(condi,block1,block2);;
let abs = [(1,Read("n"));(2,ifs);(6,Print(Var("res")))];;
reprint_polish abs 0;
printf "\n";;

(***********************************************************************)

let read_polish (filename:string) : program = 
  let program = read_file filename in 
    let rec number_lines (prg: string list) acc : (position * string ) list =
      match prg with 
      |[] -> failwith("Empty line")
      |e::l -> (acc,e)::(number_lines l (acc+1)) in
    let rec browse_program (program:string list) (acc:int) = 
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
