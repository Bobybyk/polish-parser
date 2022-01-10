open Printf;;
open Types;;

let add_env name list_var : (name * int) list =
  if List.mem_assoc name list_var 
    then list_var
  else (name, 1)::list_var;; 

(* let rec eval_type (e:expr) list_var list_var_unint =
  match e with
    | Num(i) -> list_var
    | Var(n) -> if List.mem_assoc n list_var then list_var else (print_string ("Var "^n^" can be not initialised\n") ; list_var)
    | Op(o, e1, e2) -> eval_type e1 (eval_type e2 list_var list_var_unint) list_var_unint *)

(* let check_var_cond (condition:cond) list_var list_var_unint =
  match condition with (val1, comp_type, val2) -> eval_type val1 (eval_type val2 list_var list_var_unint) list_var_unint *)
 
let rec eval_instr_under_block (instr:instr) list_var_unint : ((name * int) list) =
  match instr with 
    | Set(n,e) -> add_env n list_var_unint
    | Read(n) -> add_env n list_var_unint
    | Print(e) -> list_var_unint
    | If(c, b1, b2) -> browse_block b1 (browse_block b2 list_var_unint)
    | While(c, d) -> (browse_block d list_var_unint)

and browse_block (block:block) list_var_unint : ((name * int) list) =
  match block with
    | [] -> list_var_unint
    | instr::program' -> browse_block program' (eval_instr_under_block (snd instr) list_var_unint)

let eval_instr (instr:instr) list_var list_var_unint : (((name * int) list) * ((name * int) list)) =
  match instr with 
    | Set(n,e) -> ((add_env n list_var) , list_var_unint)
    | Read(n) -> ((add_env n list_var) , list_var_unint)
    | Print(e) -> (list_var , list_var_unint) 
    | If(c, b1, b2) -> ((browse_block b2 (browse_block b1 list_var)), list_var_unint)
    | While(c, d) -> ((browse_block d list_var), list_var_unint)

let rec print_list_assoc list_var : unit =
  match list_var with
  | [] -> ()
  | e::l' -> printf "%s " (fst e) ; print_list_assoc l'

let rec browse_program_vars (program:program) list_var list_var_unint : unit =
  match program with
    | [] -> print_list_assoc list_var ; printf "\n" ; print_list_assoc list_var_unint ; printf "\n"
    | instr::program' -> let p = eval_instr (snd instr) list_var list_var_unint in browse_program_vars program' (fst p) (snd p);;