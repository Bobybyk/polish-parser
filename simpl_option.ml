open Types;;
open Printf;;

let is_num (e:expr) : bool = 
        match e with
        | Num(i) -> true
        | _ -> false;;

let plus_num (a:expr) (b:expr) : int =
        match a with
        | Num(e1) -> (match b with
                        | Num(e2) -> (e1 + e2)
                        | _ -> failwith("Error plus_num b")) 
        | _ -> failwith("Error plus_num a");;

let rec simplify_expr (e: expr) : expr =
        match e with
        | Op(o,e1,e2) -> if (is_num (simplify_expr e1)) && (is_num (simplify_expr e2)) 
                        then Num((plus_num e1 e2))
                        else e
        | _ -> e;;

let simplify_aux (p: (position * instr) ) : (position * instr) = 
        match (snd p) with
        | Set(n,e) -> ((fst p),Set(n,(simplify_expr e)))
        | _ -> p;;

let rec simplify (p:program) : program = 
        match p with
        | [] -> failwith("Empty program")
        | e::[] -> (simplify_aux e)::[]
        | e::l -> (simplify_aux e)::(simplify l);;
