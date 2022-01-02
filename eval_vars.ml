open Printf;;
open Types;;

let add_env name list_var : (name * int) list =
  if List.mem_assoc name list_var 
    then list_var
  else (name, 1)::list_var;; 

let rec eval_type (e:expr) list_var : (name * int) list =
  match e with
    | Num(i) -> list_var
    | Var(n) -> if List.mem_assoc n list_var then list_var else (print_string ("Var "^n^" can be not initialised\n") ; list_var)
    | Op(o, e1, e2) -> eval_type e1 (eval_type e2 list_var)

let check_var_cond (condition:cond) list_var : (name * int) list =
  match condition with (val1, comp_type, val2) -> eval_type val1 (eval_type val2 list_var)

let rec eval_instr_under_block (instr:instr) list_var : (name * int) list =
  match instr with 
    | Set(n,e) -> list_var
    | Read(n) -> list_var
    | Print(e) -> eval_type e list_var
    | If(c, b1, b2) -> (check_var_cond c (browse_block b1 (browse_block b2 list_var)))
    | While(c, d) -> (check_var_cond c (browse_block d list_var))

and browse_block (block:block) list_var : (name * int) list =
  match block with
    | [] -> list_var
    | instr::program' -> browse_block program' (eval_instr_under_block (snd instr) list_var)

let eval_instr (instr:instr) list_var : (name * int) list =
  match instr with 
    | Set(n,e) -> add_env n list_var
    | Read(n) -> add_env n list_var
    | Print(e) -> eval_type e list_var
    | If(c, b1, b2) -> (browse_block b2 (browse_block b1 (check_var_cond c list_var)))
    | While(c, d) -> (check_var_cond c (browse_block d list_var))

let rec browse_program_vars (program:program) list_var : unit =
  match program with
    | [] -> ()
    | instr::program' -> browse_program_vars program' (eval_instr (snd instr) list_var);;