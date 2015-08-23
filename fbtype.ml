open Fbast;;

(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *) 
(* let typecheck e = raise Fbdk.TypecheckerNotImplementedException;; *)

exception TypeMismatch;;
exception TypeError;;
exception CycleException;;


let rec union lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | (t1, t2) :: tl -> if (List.mem (t1, t2) lst2) || (List.mem (t2, t1) lst2) then 
                                    union tl lst2
                                else
                                    union tl ((t1, t2) :: lst2)
;;

let rec createFreshTypeVar nameList code =
    if List.mem (Char.escaped (Char.chr code)) nameList then
        createFreshTypeVar nameList (code + 1)
    else
        Char.escaped (Char.chr code)
;;


let rec merge (type1, type2) lst =
    match lst with
    | [] -> []
    | (type3, type4) :: tl ->
        if (type1 <> type3 || type2 <> type4) then
    (   if  type2 = type3 then
            union [(type1, type4)] (merge (type1, type2) tl) 
        else if type2 = type4 then
            union [(type1, type3)] (merge (type1, type2) tl)    
        else if type1 = type4 then
            union [(type2, type3)] (merge (type1, type2) tl)    
        else if type1 = type3 then
            union [(type2, type4)] (merge (type1, type2) tl)    
            else 
            merge (type1, type2) tl )   
    else 
        merge (type1, type2) tl
;;

let rec addEquation lst =
    match lst with
    | [] -> []
    | hd :: tl ->
        let lst1 =
        (   match hd with
        | (TArrow (type1, type2), TArrow (type3, type4)) -> 
            [(type1, type3); (type2, type4)]
        | _ -> []   )
        in
            let lst2 = merge hd tl in
                union (union [hd] (union lst1 lst2)) (addEquation tl)
;;

let isConsistent lst =
    let rec isSameType pair =
        match pair with
        | (TVar _, _) | (_, TVar _) -> true
        | (TInt, TInt) | (TBool, TBool) -> true
        | (TInt, _) | (_, TInt) | (TBool, _) | (_, TBool) -> false
        | (TArrow (type1, type2), TArrow (type3, type4)) ->
            (isSameType (type1, type3)) && (isSameType (type2, type4))
    in
        List.for_all isSameType lst
;;

let rec closure lst =
    let lst1 = addEquation lst in
        let lst2 = addEquation lst1 in
            if (List.length lst1) = (List.length lst2) then
                lst1
            else
                closure lst2
;;


let rec replace alpha eq =
    match alpha with
    | TInt | TBool -> alpha
    | TArrow (type1, type2) ->
        TArrow ((replace type1 eq), (replace type2 eq))
    | TVar t ->
    (   match eq with
        | (TVar a, TVar b) ->
            if (alpha = TVar a) && a < b then
                TVar b
            else if (alpha = TVar b) && a > b then
                TVar a
            else
                alpha   
        | (TVar a, b) | (b, TVar a) ->
            if (alpha = TVar a) then
                replace b eq
            else
                alpha       
        | _ -> alpha    )
;;

let rec solveEquation alpha lst =
    match lst with
    | [] -> alpha
    | hd :: tl ->
        solveEquation (replace alpha hd) tl
;;

let rec solve alpha lst =
    let a = solveEquation alpha lst in
        let b = solveEquation a lst in
            if a <> b then
                solve a lst
            else
                a
;;



let rec collectVar e =
    match e with
    | TInt | TBool -> []
    | TArrow (a, b) ->
        (collectVar a) @ (collectVar b)
    | TVar a ->
        [a]
;;

let rec findAdj node lst =
    match lst with
    | [] -> []
    | (a, b) :: tl ->
         if (TVar node) = a then
            (match b with
            | TArrow(_, _) ->
                (collectVar b) @ (findAdj node tl)
            | _ ->
                findAdj node tl )
        else
            findAdj node tl
;;



let rec dfs node eq visitedList =
    if  (List.mem node visitedList) then
        false
    else

        (let adjList = findAdj node eq in
            if (List.exists (fun x -> List.mem x (node::visitedList)) adjList) then
                false 
            
            else
                dfsList adjList eq (node :: visitedList)  )

and

dfsList lst eq visitedList =
    match lst with
    | [] -> true
    | hd :: tl ->
        (dfs hd eq visitedList) && (dfsList tl eq visitedList)
;;

let rec checkCycle eq nameList =
    match nameList with
    | [] -> true
    | hd :: tl ->
        (dfs hd eq []) && (checkCycle eq tl)
;;


let rec recTypeCheck gamma e nameList =
    match e with
    | Int _ -> (TInt, [], nameList)
    | Bool _ -> (TBool, [], nameList)
    | Var ident -> 
        if (List.mem_assoc ident gamma) then
            ((List.assoc ident gamma), [], nameList)
        else
            raise TypeError

    | Plus (e1, e2) | Minus (e1, e2) -> 
        let (type1, eq1, nameList1) = recTypeCheck gamma e1 nameList in
            let (type2, eq2, nameList2) = recTypeCheck gamma e2 nameList1 in
                (TInt, (union (union eq1 eq2) [(type1, TInt); (type2, TInt)]), nameList2)
    | Equal (e1, e2) ->
        let (type1, eq1, nameList1) = recTypeCheck gamma e1 nameList in
            let (type2, eq2, nameList2) = recTypeCheck gamma e2 nameList1  in
                (TBool, (union (union eq1 eq2) [(type1, TInt); (type2, TInt)]),  nameList2)
    | And (e1, e2) | Or (e1, e2) ->
        let (type1, eq1, nameList1) = recTypeCheck gamma e1 nameList in
            let (type2, eq2, nameList2) = recTypeCheck gamma e2 nameList1 in 
                (TBool, (union (union eq1 eq2) [(type1, TBool); (type2, TBool)]), nameList2)
    | Not e ->
        let (type1, eq1, nameList1) = recTypeCheck gamma e nameList in
            (TBool, (union eq1 [(type1, TBool)]), nameList1 )       
    | If (e1, e2, e3) ->
        let newName = createFreshTypeVar nameList 97 in
            let alpha = TVar newName in
                let (type1, eq1, nameList1) = recTypeCheck gamma e1 (newName :: nameList) in
                    let (type2, eq2, nameList2) = recTypeCheck gamma e2 nameList1 in
                        let (type3, eq3, nameList3) = recTypeCheck gamma e3 nameList2 in
                            (alpha, 
                            (union (union (union eq1 eq2) eq3) 
                                   [(type1, TBool); (type2, alpha); (type3, alpha)] ),
                            nameList3
                            )
    | Function (Ident i, e) ->
        let newName = createFreshTypeVar (i :: nameList) 97 in
            let alpha = TVar newName in
                let newGamma = if (List.mem (Ident i, alpha) gamma) then gamma 
                               else ((Ident i, alpha) :: gamma) in
                    let (type1, eq1, nameList1) = recTypeCheck newGamma e (newName :: i :: nameList) in
                        (TArrow (alpha, type1), eq1, nameList1)

    | Appl (e1, e2) ->
        let newName = createFreshTypeVar nameList 97 in
            let alpha = TVar newName in
                let (type1, eq1, nameList1) = recTypeCheck gamma e1 (newName :: nameList) in
                    let (type2, eq2, nameList2) = recTypeCheck gamma e2 nameList1 in 
                        (alpha, (union (union eq1 eq2) [(type1, TArrow (type2, alpha))]), nameList2 )

    | _ -> raise TypeError  
;;


let rec recordName e nameList =
    match e with
    | TInt | TBool -> nameList
    | TArrow (a, b) ->
        recordName b (recordName a nameList)
    | TVar a ->
        if (List.mem_assoc a nameList) then
            nameList
        else
            (a, Char.escaped (Char.chr (List.length nameList + 97  ))) :: nameList
;;

let rec changeName e nameList =
    match e with
    | TInt | TBool -> e
    | TArrow (a, b) ->
        TArrow (changeName a nameList, changeName b nameList)
    | TVar a ->
        if (List.mem_assoc a nameList) then
            TVar (List.assoc a nameList)
        else
            e
;;

let typecheck e =
    let (t, eq, nameList) =  recTypeCheck [] e [] in
        let lst = closure eq in
            if isConsistent lst then    
                if checkCycle lst nameList then
                    let ttype = solve t lst in
                        changeName ttype (recordName ttype [])
                else 
                    raise CycleException
            else
                raise TypeMismatch
;;

