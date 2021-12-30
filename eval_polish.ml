open Printf
open Types

(******************************eval_polish******************************)

let eval_read name env = let reponse = read_int() in (name, reponse)::env;;

let eval_print (expr:expr) list_var : unit =
  match expr with
    | Num(i) -> printf "%d " i
    | Var(n) -> printf "%d " (List.assoc n list_var)
    | _ -> ()

let rec eval_type (e:expr) list_var =
  match e with
    | Num(i) -> i
    | Var(n) -> List.assoc n list_var 
    | Op(o, e1, e2) -> compute o e1 e2 list_var

and compute (op:op) (e1:expr) (e2:expr) list_var : int =
  match op with
    | Add -> eval_type e1 list_var + eval_type e2 list_var
    | Sub -> eval_type e1 list_var - eval_type e2 list_var
    | Mul -> eval_type e1 list_var * eval_type e2 list_var
    | Div -> eval_type e1 list_var / eval_type e2 list_var
    | Mod -> eval_type e1 list_var mod eval_type e2 list_var

let eval_set (name:name) (expr:expr) list_var : (name * int) list =
  if List.mem_assoc name list_var then 
    (match expr with
      | Num(i) -> (name, i)::List.remove_assoc name list_var 
      | Var(n) -> (name, (List.assoc n list_var))::(name, List.assoc n list_var)::List.remove_assoc name list_var
      | Op(o, e1, e2) -> (name, (compute o e1 e2 list_var))::List.remove_assoc name list_var)
  else 
    (match expr with
      | Num(i) -> (name, i)::list_var
      | Var(n) -> (name, (List.assoc n list_var))::list_var
      | Op(o, e1, e2) -> (name, (compute o e1 e2 list_var))::list_var)

let eval_instr (instr:instr) list_var : (name * int) list =
  match instr with 
    | Set(n,e) -> eval_set n e list_var
    | Read(n) -> eval_read n list_var 
    | Print(e) -> eval_print e list_var ; list_var
    | If(c, b, b2) -> failwith("eval If to do") 
    | While(c, d) -> failwith("eval while to do")

let rec browse_program (program:program) list_var : unit =
  match program with
    | [] -> ()
    | instr::program' -> browse_program program' (eval_instr (snd instr) list_var);;