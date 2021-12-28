open Printf
open Types

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