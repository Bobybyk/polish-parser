open Types;;
open Printf;;

let is_num (e:expr) : bool = 
        match e with
        | Num(i) -> true
        | _ -> false;;

let is_empty (l: 'a list) : bool =
        match l with
        | [] -> true
        | _ -> false;;

let calculate_num (e:expr) : expr =
        match e with 
        | Op(o,e1,e2) ->
                        let a = (match e1 with 
                                |Num(a) -> a 
                                | _ ->failwith("Calculate_num a : empty expression")) and
                        b = (match e2 with
                                | Num(b) -> b
                                |_->failwith("Calculate_num b : empty expression")) in 
                (match o with
                | Add -> Num((a + b))
                | Sub -> Num((a - b))
                | Mul -> Num((a * b))
                | Div -> Num((a / b))
                | Mod -> Num((a mod b)))
        | _ -> failwith("calculate num");;
        
let rec simplify_expr (e: expr) : expr =
        (match e with
        | Op(o,e1,e2) -> if (is_num (simplify_expr e1)) && (is_num (simplify_expr e2)) 
                         then calculate_num e 
                         else failwith("TODO")
        (* TODO: detect op type and calculate result accordingly *)
        | _ -> e);;

let rec simplify_aux (p: (position * instr) ) : (position * instr) = 
        match (snd p) with
        | Set(n,e) -> ((fst p),Set(n,(simplify_expr e)))
        | If(c,b1,b2) -> if (is_empty b2) 
                                then ((fst p), If(c,(simplify b1),[]))
                                else ((fst p),If(c,(simplify b1),(simplify b2)))
        | While(c,b) -> ((fst p),While(c,(simplify b)))
        | _ -> p

and simplify (p:program) : program = 
        match p with
        | [] -> failwith("Empty program")
        | e::[] -> (simplify_aux e)::[]
        | e::l -> (simplify_aux e)::(simplify l);;
