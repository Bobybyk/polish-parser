open Printf;;
open Types;;

let add_env name list_var : (name * int) list =
  if List.mem_assoc name list_var 
    then (name, 1)::List.remove_assoc name list_var
  else (name, 1)::list_var;; 

let rec eval_type (e:expr) list_var =
  match e with
    | Num(i) -> list_var
    | Var(n) -> if List.mem_assoc n list_var then list_var else printf ("Var "^n^" can be not initialised") && list_var
    | Op(o, e1, e2) -> eval_type e1 list_var && eval_type e2 list_var

let check_var_cond (condition:cond) list_var : (name * int) list =
  match cond with (val1, comp_type, val2) -> list_var

let rec browse_block (block:block) list_var : (name * int) list =
  match block with
    | [] -> list_var
    | instr::program' -> list_var

let rec eval_instr (instr:instr) list_var : (name * int) list =
  match instr with 
    | Set(n,e) -> add_env n list_var
    | Read(n) -> add_env n list_var
    | Print(e) -> list_var
    | If(c, b1, b2) -> browse_block b1 (check_var_cond c list_var) 
    | While(c, d) -> list_var

let rec browse_program (program:program) list_var : unit =
  match program with
    | [] -> ()
    | instr::program' -> browse_program program' (eval_instr (snd instr) list_var);;