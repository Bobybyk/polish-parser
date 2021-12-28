open Printf;;
open Types;;
open Reprint;;
open Read_polish;;
  
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

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
  loop [];;

(***********************************************************************)

(* tests *)
(* let file_content = read_file "exemples/abs.p";; *)
(* let () = List.iter (printf "%s ") file_content;; *)

(* absolute value function *)
let condi = (Var("n"),Lt,Num(0));;
let block1 = [(3,Set("res",Op(Sub,Num(0),Var("n"))));(0,Print(Var("Test")))];;
let block2 = [(5,Set("res",Var("n")))];;
let ifs = If(condi,block1,block2);;
let abs = [(1,Read("n"));(2,ifs);(6,Print(Var("res")))];;
let test = [(1,Set("t",Var("n"))); (6,Print(Var("t")))];;
(* reprint_polish abs 0;
printf "\n";; *)

(******************************eval_polish******************************)

let eval_read name env = let reponse = read_int() in (name, reponse)::env;;

let eval_print (expr:expr) list_var : unit =
  match expr with
    | Num(i) -> printf "%d " i
    | Var(n) -> printf "%s " n
    | _ -> ()

let eval_set (name:name) (expr:expr) list_var =
  if List.mem_assoc name list_var then 
    (match expr with
      | Num(i) -> (name, i)::List.remove_assoc name list_var
      | Var(n) -> failwith("set var to do")
      | Op(o, e1, e2) -> failwith("set op to do"))
  else 
    (match expr with
      | Num(i) -> (name, i)::list_var
      | Var(n) -> failwith("set var to do")
      | Op(o, e1, e2) -> failwith("set op to do"))

let eval_instr (instr:instr) list_var =
  match instr with 
    | Set(n,e) -> eval_set n e list_var
    | Read(n) -> eval_read n list_var
    | Print(e) -> eval_print e list_var ; list_var
    | If(c, b, b2) -> failwith("eval If to do")
    | While(c, d) -> failwith("eval while to do")

let rec browse_program (program:program) list_var : unit =
  match program with
    | [] -> failwith("maybe to do")
    | instr::program' -> browse_program program' (eval_instr (snd instr) list_var);;

(***********************************************************************)

let print_polish (p:program) : unit = reprint_polish p 0;;

let eval_polish (p:program) : unit = (* browse_program program [] *)
  failwith "TODO"

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
