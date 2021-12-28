open Types;;

let simplify_expr (e: expr) : expr =
        match e with
        | Op(o,e1,e2) -> failwith("TODO")
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
