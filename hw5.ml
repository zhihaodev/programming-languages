(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*
 
  Name                        :	Zhihao Cao
  List of team Members        :	Jian Jin
  List of other collaborators :

*) 


type label = Lab of string

type fbtype =
    TInt | TBool | TArrow of fbtype * fbtype | TRecord of (label * fbtype) list
    

let rec is_subtype t1 t2 =
	if t1 = t2 then
		true
	else
		match (t1, t2) with
		| (TRecord a, TRecord b) -> 
			if (List.length a) < (List.length b) then
				false
			else
				scan_record a b
		| (TArrow (a, b), TArrow (c, d)) ->
			(is_subtype c a) && (is_subtype b d)
		| _ -> false
		  
and

scan_record r1 r2 =
	match r2 with
	| [] -> true
	| hd :: tl ->
			if not (check_each_field r1 hd) then
				false
			else
				scan_record r1 tl

and

check_each_field r pair =
	match r with
	| [] -> false
	| hd :: tl ->
		let (l1, t1) = hd in
			let (l2, t2) = pair in
				if l1 = l2 then
					is_subtype t1 t2
				else
					check_each_field tl pair
;;


(*    
# let r1 = TRecord [(Lab "x", TInt)] ;;
# let r2 = TRecord [(Lab "x", TInt); (Lab "y", TBool)] ;;
# let r3 = TRecord [(Lab "x", TInt); (Lab "y", TInt)] ;;
# let r4 = TRecord [] ;;
# let r5 = TRecord [(Lab "x", TInt); (Lab "y", TBool); (Lab "z", TRecord [(Lab "x", TInt)])] ;;
# let r6 = TArrow(r1, r2) ;;
# let r7 = TArrow(r4, r5) ;;
# is_subtype r2 r1 ;;
- : bool = true
# is_subtype r3 r2 ;;
- : bool = false
# is_subtype r7 r6 ;;
- : bool = true
*)


2.


τ ::= ... | [τ]


  Γ ⊢e_1 : τ,  ...,  Γ ⊢e_n : τ
---------------------------------
     Γ ⊢[e_1; ...; e_n] : [τ]               



 Γ⊢e1 : τ,    Γ,[[ ]]:[τ] ⊢e2 : [τ]    
-----------------------------------
         Γ⊢ e1 ++ e2 : [τ]



Γ ⊢e : [τ1],    Γ ⊢e1 : τ2,    Γ,h:τ1, t:[τ1] ⊢e2 : τ2  
-----------------------------------------------------------
   Γ ⊢Match e With [[ ]] -> e1 | (h ++ t) -> e2 :  τ2 



