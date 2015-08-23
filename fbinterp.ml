open Fbast;;

(*
 * Replace this with your interpreter code.
 *)

exception TypeMismatch;;
exception NotClosed;;

let check_closed e =
    let rec check e identList =
        match e with
            Not e -> check e identList
        |   And (e1, e2) -> check e1 identList && check e2 identList
        |   Or (e1, e2) -> check e1 identList && check e2 identList
        |   Plus (e1, e2) -> check e1 identList && check e2 identList
        |   Minus (e1, e2) -> check e1 identList && check e2 identList
        | Equal (e1, e2) -> check e1 identList && check e2 identList
        | If (e1, e2, e3) -> check e1 identList && check e2 identList && check e3 identList
        | Appl (e1, e2) -> check e1 identList && check e2 identList
        | Function (ident, expr) -> 
                check expr (ident :: identList)
        | LetRec (f, x, e1, e2) ->
                check e1 (f :: x :: identList) && check e2 (f :: x :: identList)
        
        | Var e -> List.mem e identList
                    
        | _ -> true
        
    in check e []
;;

(*e[v/x]*)
let subst e v x =
    let rec sub e =
        match e with
            Not e -> Not (sub e)
        |   And (e1, e2) -> And (sub e1, sub e2)
        |   Or (e1, e2) -> Or (sub e1, sub e2)  
        |   Plus (e1, e2) -> Plus (sub e1, sub e2)
        |   Minus (e1, e2) -> Minus (sub e1, sub e2)
        | Equal (e1, e2) -> Equal (sub e1, sub e2)
        | If (e1, e2, e3) -> If (sub e1, sub e2, sub e3)
        | Appl (e1, e2) -> Appl (sub e1, sub e2)
        | Int e -> Int e
        | Bool e -> Bool e
        | Function (ident, expr) ->
                if ident = x then 
                    e
                else
                    Function (ident, sub expr)  
        
        |   LetRec (f, xx, e1, e2) ->
                if f = x then
                    e
                else if xx = x then
                    LetRec (f, xx, e1, sub e2)
                else
                    LetRec (f, xx, sub e1, sub e2)
        
        |   Var ident ->
                if ident = x then
                    v
                else
                    e
                    
    in sub e
;;  


let eval e = 
    
    let rec recEval e = 
    match e with
        Not e ->
        (   match recEval e with
            Bool x -> Bool (not x)
        | _ -> raise TypeMismatch   )
    
        |   And (e1, e2) -> 
            (   match (recEval e1, recEval e2) with
                    (Bool x, Bool y) -> Bool (x && y)
                | _ -> raise TypeMismatch )
        
        | Or (e1, e2) ->
            (   match (recEval e1, recEval e2) with
                    (Bool x, Bool y) -> Bool (x || y)
                | _ -> raise TypeMismatch   )
        
        | Plus (e1, e2) ->
            (   match (recEval e1, recEval e2) with
                    (Int x, Int y) -> Int (x + y)
                |   _ -> raise TypeMismatch )
    
        |   Minus (e1, e2) ->
            (   match (recEval e1, recEval e2) with
                    (Int x, Int y) -> Int (x - y)
                |   _ -> raise TypeMismatch )

        | Equal (e1, e2) ->
            (   match (recEval e1, recEval e2) with
                    (Int x, Int y) -> Bool (x = y)
                |   _ -> raise TypeMismatch )
        
        | If (e1, e2, e3) ->
            (   match recEval e1 with
                    Bool true -> recEval e2
                |   Bool false -> recEval e3
                |   _ -> raise TypeMismatch )
        
        | Appl (e1, e2) ->
            ( match recEval e1 with
                    Function (ident, expr) ->
                        recEval (subst expr (recEval e2) ident) 
                |   _ -> raise TypeMismatch ) 

        | LetRec (f, x, e1, e2) ->  
                recEval (subst e2 (Function (x, subst e1 (  LetRec (f, x, e1, Var f)  ) f)) f)      
    
        | _ -> e
        
    in 
    
    if (check_closed e ) then 
        recEval e 
    else 
        raise NotClosed
;;


