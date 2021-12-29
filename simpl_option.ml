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

let get_value (e:expr) : int = 
        match e with
        | Num(e) -> e
        | _ -> failwith("Error: not a num");;

let calculate_num (e:expr) : expr =
        match e with 
        | Op(o,e1,e2) -> let a = get_value e1 and
                        b =  get_value e2 in
                (match o with
                | Add -> Num((a + b))
                | Sub -> Num((a - b))
                | Mul -> Num((a * b))
                | Div -> Num((a / b))
                | Mod -> Num((a mod b)))
        | _ -> failwith("calculate num: no Op found");;

let calculate_constants (e:expr) : expr =
        match e with 
        | Op(o,e1,e2) -> if is_num e1 && is_num e2 then calculate_num e         (* if both sides are a Num, simplify the expr *)
                         else (match o with                                     (* else check for the Op *)
                                | Add -> if not(is_num e1) && is_num e2         (* if Add, simplify only when adding zero *)
                                                then if (get_value e2) = 0 then e1
                                                else e
                                        else if (is_num e1) && not(is_num e2)
                                                then if (get_value e1) = 0 then e2
                                                else e
                                        else e
                                | Div -> if (is_num e1) && not(is_num e2) (* if Div, simplify when dividing zero *)
                                                then if (get_value e1) = 0 then Num(0)
                                                else e
                                        else e
                                | Mul -> if not(is_num e1) && is_num e2         (* if Mul, simplify when multiplying by one and zero*)
                                                then if (get_value e2) = 0 then Num(0) 
                                                else if (get_value e2) = 1 then e1
                                                else e
                                        else if (is_num e1) && not(is_num e2)
                                                then if (get_value e1) = 0 then Num(0)
                                                else if (get_value e1) = 1 then e2
                                                else e
                                        else e

                                | _ -> failwith("TODO")) 
        | _ -> e;;
        
let rec simplify_expr (e: expr) : expr =
        (match e with
        | Op(o,e1,e2) -> let ne1 = (is_num (simplify_expr e1)) and
                             ne2 =  (is_num (simplify_expr e2)) in
                         if ne1 && ne2          (* if both expressions are a Num *) 
                         then 
                                 calculate_num e 
                         else if ne1 && not(ne2)        (* if only the left expr is a Num *)
                         then 
                                (calculate_constants e) 
                         else if not(ne1) && ne2        (* if only the right expr is a Num *)
                         then
                               (calculate_constants e) 
                         else                           (* if neither are a Num *)
                                e 
        | _ -> e);;

let simplify_condition (c:cond) : cond = 
        match c with
        | (e1,c,e2) -> ((simplify_expr e1),c,(simplify_expr e2));;

let rec simplify_aux (p: (position * instr) ) : (position * instr) = 
        match (snd p) with
        | Set(n,e) -> ((fst p),Set(n,(simplify_expr e)))
        | If(c,b1,b2) -> if (is_empty b2) 
                                then ((fst p), If((simplify_condition c),(simplify b1),[]))
                                else ((fst p),If((simplify_condition c),(simplify b1),(simplify b2)))
        | While(c,b) -> ((fst p),While((simplify_condition c),(simplify b)))
        | _ -> p

and simplify (p:program) : program = 
        match p with
        | [] -> failwith("Empty program")
        | e::[] -> (simplify_aux e)::[]
        | e::l -> (simplify_aux e)::(simplify l);;
