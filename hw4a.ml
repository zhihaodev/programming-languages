600.426 - Programming Languages
JHU Spring 2015
Homework 4 Part 1 (40 points)

--------------------------------------------------------------------------------------------------
HEADER: PLEASE FILL THIS IN
--------------------------------------------------------------------------------------------------

Name                  : Zhihao Cao
List of team Members  : Hefeng Sun
List of discussants   :

--------------------------------------------------------------------------------------------------
  Macros and expansion
--------------------------------------------------------------------------------------------------

(*
  1. We have discussed macros over Fb extensively in the class and in the book. Within our context, macros
     are used to convert useful high-level structures in to plain Fb code. For the most part, macro expansion
     is a fairly mechanical process and instead of doing all these expansions by hand, it would be useful to
     automate the process. Like in the case of the interpreter, macro expansion can be expressed as a transformation 
     on the Fb ast. In all cases expansion occurs before an expression is evaluated using the interpreter. 
     
     However most macros from the textbook are not *directly* definable in FbDk since they tend to use fancy syntax - 
     e.g. Pairs are defined by the syntax Pr (e1,e2). Defining this sort of syntax requires additional parsing work. 
     However if we are willing to restrict ourselves to a simpler function call form, we can get a lot further with 
     the standard FbDk. Here is an redefinition of a *lazy* pair macro (where --> is used to indicate a definition):
     
       lpair  --> Function e1 -> Function e2 -> Function x -> x e1 e2
       left   --> Function p -> p (Function x -> Function y -> x)
       right  --> Function p -> p (Function x -> Function y -> y)
       
     Notice that the expansions for all 3 macros use the Fb function notation. When expanded (lpair (3+1) 4) produces 
     (Function x -> x (3+1) 4). Remember that expansion occurs before eval; so this substitution process is implemented
     by the macro expansion routine itself.
     
     Consider this version of the Let macro:
     
       let --> Function variable -> Function expr1 -> Function expr2 -> (Function variable -> expr2) expr1 
       
     You can call it thus: let x 3 (x + 10). This expression expands to (Function x -> x + 10) 3. Observe that in
     the expansion, the function identifier in the macro body has also been substituted. This is different from what
     happens in the interpreter's substitution function.
     
     Using ||==> to indicate a macro expansion followed by evaluation, we have
       left (lpair (3+4) 4) ||==> 7
       right (lpair 3 (lpair True False)) ||==> Function p -> p True False 
     
     For this question, we will implement a simple macro expander.
     
     We define a macro as a triple - (name, arity, code) - where name is an Ident representing the name of the macro,
     arity is the number of arguments expected and code is its expansion in the form of an AST. To use a macro, 
     we use function call syntax with the identifier of the macro. Note that a macro is allowed to use macros in its 
     expansion. If a macro claims to take 2 arguments, then its code will have the form: Fun arg1 -> Fun arg2 -> <the body>.
     
     We will make a few simplifying assumptions:
      1. The macro name has precedence over your program's variables. i.e. If you find an identifier in the expression
         you are expanding that is also present in our list of macros, you can treat it as a macro name.
      2. All macro arguments must be applied immediately. You cannot partially apply macros or use the name without
         applying sufficient number of arguments.
         
     Note: This is still a fairly rudimentary system for macro expansion. For example, it is prone to variable naming
     conflicts if we are not careful in writing out our macros. The example macro list uses some strange naming conventions
     to mitigate *some* of that. A "real" system will have to deal with a plethora of other issues like this.
     
     Note: For this section, you can assume that functions parse, eval, pp and ppeval from Fbdktoploop.ml is available 
     to you.
     
     Hints:
       0. Helper functions help a lot!
       1. Macro uses (depending on the arity) will show up in the AST as a nested Appl like so: 
          Appl(Appl(Var (Ident "lpair"), Int 3), Int 4). You need a function to detect such sequences.
       2. Macro definitions in the macro list have a (potentially) nested form: Function(id, (Function id', ..)) where id
          and id' are formal parameters in the macro code. A main task for the expansion routine is to "align" the values 
          from the macro use site to the formal parameters and perform a substitution in the macro body. 
       3. For the last step above, you can use the substitution function you wrote for Homework-3 as a base. But it will 
          need some tweaks before all the examples work, especially Let.
       4. Start with getting the simpler examples to work and then work your way up.   
          
     [20 Points]      
*)

(*     
   Given a list of macros and a program as an ast, write an Ocaml function to return an ast with all the macros 
   expanded.
*)     
                                                                                    
exception Failure;;     

let subst1 e v x =
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
                (   match v with
                        Var i -> Function(i, sub expr)
                    |   _ -> e  )
                else
                    Function(ident, sub expr) 
        
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
    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
let rec findMacros macros ident arity = match macros with
                                                                    [] -> (Ident "notFound", 0, Int 0)
                                                                | hd :: tl ->
                                                                        let (n, a, c) = hd in
                                                                            if Var(n) = ident && a - 1 = arity || Var(n) = ident && a = 0 && arity = 0  then
                                                                                hd
                                                                            else
                                                                                findMacros tl ident arity
;;

let rec substitute code valueList identList length = 
                        if length = 0 then
                            code
                        else
                            substitute (subst1 code (List.nth valueList (length - 1)) (  List.nth identList (length - 1)  ) ) valueList identList (length - 1)
;;

let rec alignValue macro valueList identList =  let (n, length, code) = macro in
                                                                                        match code with
                                                                                            Function (ident, expr) -> 
                                                                                                if length = 0 then
                                                                                                    Function(ident, alignValue (n, length, expr) valueList identList)
                                                                                                else
                                                                                                    alignValue (n, (length-1), expr) valueList ( identList @ [ident])
                                                                                        | e ->
                                                                                                substitute code valueList identList (List.length valueList)
;;                                                                          


    
let rec traverseProgram macros program = 
                                                                                        match program with
                                                                                            Not e -> Not (traverseProgram macros e)
                                            |   And (e1, e2) -> And (traverseProgram macros e1, traverseProgram macros e2)
                                            |   Or (e1, e2) -> Or (traverseProgram macros e1, traverseProgram macros e2)
                                            |   Plus (e1, e2) -> Plus (traverseProgram macros e1, traverseProgram macros e2)
                                            | Minus (e1, e2) -> Minus (traverseProgram macros e1, traverseProgram macros e2)
                                            | Equal (e1, e2) -> Equal (traverseProgram macros e1, traverseProgram macros e2)
                                            | If (e1, e2, e3) -> If (traverseProgram macros e1, traverseProgram macros e2, traverseProgram macros e3)
                                            | Int e -> Int e
                                            | Bool e -> Bool e
                                            | Function (ident, expr) ->
                                                Function (ident, traverseProgram macros expr)    
                                            |   LetRec (f, xx, e1, e2) ->
                                                LetRec (f, xx, traverseProgram macros e1, traverseProgram macros e2)
                                            |   Var ident ->
                                                                                let (n, a, c) = findMacros macros program 0 in
                                                                                                        if n <> (Ident "notFound") then
                                                                                                            traverseProgram macros c
                                                                                                        else
                                                                                                            program
                                                                        | Appl (e1, e2) -> 
                                                                                substMacro macros program
                                                                                                
and

substMacro macros oprogram = 
let rec sub macros program arity valueList = match program with
                                                                                                Appl (e1, e2) ->
                                                                                                    let (n, a, c) = findMacros macros e1 arity in
                                                                                                        if n <> (Ident "notFound") then
                                                                                                            traverseProgram macros (alignValue (n, a, c) ((e2) :: valueList) []) 
                                                                                                        else
                                                                                                            sub macros e1 (arity + 1) ((e2) :: valueList) 

                                                                                                            
                                                                                            |   _ -> match oprogram with
                                                                                                    Appl(e1, e2) -> Appl(traverseProgram macros e1, traverseProgram macros e2)
                                                                                                    |   _ -> raise Failure
in
sub macros oprogram 0 []                                                                                
;;

let expand macros program =  traverseProgram macros program ;;
                                                                                                                                                                                                                                                                                                                    

let expand_eval macros program = eval  ( expand macros program ) ;;



(* 
let macro_list = [
    (Ident "lpair", 2, parse "Fun e1__ -> Fun e2__ -> Fun pr__ -> pr__ e1__ e2__") ; 
    (Ident "left", 1, parse "Fun pr__ -> pr__ (Fun e1__ -> Fun e2__ -> e1__)") ;
    (Ident "right", 1, parse "Fun pr__ -> pr__ (Fun e1__ -> Fun e2__ -> e2__)");
    (Ident "emptylist", 0, parse "lpair (lpair True 0) 0");
    (Ident "cons", 2, parse "Fun v__ -> Fun lst__ -> lpair (lpair False v__) lst__");
    (Ident "head", 1, parse "Fun lst__ -> right (left lst__)");
    (Ident "tail", 1, parse "Fun lst__ -> right lst__");
    (Ident "isempty", 1, parse "Fun lst__ -> left (left lst__)");
    (Ident "let", 3, parse "Fun var__ -> Fun expr1__ -> Fun expr2__ -> (Fun var__ -> expr2__) expr1__") 
] ;;
 
#  let code_1 = parse "(Fun p -> right (left p)) (lpair (lpair 3 (1+1)) 4)" ;;
# expand_eval macro_list code_1 ;;
- : Fbast.expr = Int 2
# let code_2 = parse "(Fun pr -> right (lpair (left pr + left pr) (right pr + right pr)))(lpair 3 4)" ;;
# expand_eval macro_list code_2 ;; 
- : Fbast.expr = Int 8
# let code_3 = parse "
    (Fun p ->
        right ( lpair (left (left p) + left (right p)) (right (left p) + right (right p)) )
    ) 
    (lpair (lpair 3 1) (lpair 2 7))" ;;
# expand_eval macro_list code_3 ;;
- : Fbast.expr = Int 8
# let code_4 = parse "(Fun l -> head (tail l))(cons 3 (cons 2 (cons 1 emptylist)))" ;;
# expand_eval macro_list code_4 ;;
- : Fbast.expr = Int 2
# let code_5 = parse "let x 10 (x + 3)" ;;
# expand_eval macro_list code_5 ;;
- : Fbast.expr = Int 13
let code_6 = parse "let x 10 (let y 2 (x+y))" ;;
# expand_eval macro_list code_6 ;;
- : Fbast.expr = Int 12
# let code_7 = parse "let x 10 (let y 2 (Function z -> x+y+z) 20)" ;;
# expand_eval macro_list code_7 ;;
- : Fbast.expr = Int 32
(* You can even bring Ocaml in to play to freely manipulate asts *)
# let appl2 f v1 v2 = Appl(Appl(f, v1), v2) ;;
# let lift_int_list lst = let cons = Var (Ident "cons") in List.fold_right (
    fun v -> fun res -> appl2 cons (Int v) res 
  ) lst (parse "emptylist") ;;
# let list_1 = lift_int_list [10;9;8;7;6] ;;
(* And for the grand finale, such as it is, we present shockingly readable code *)
# let code_8 = Appl(parse "
    Let Rec last v = Fun lst -> 
      If isempty lst Then v Else last (head lst) (tail lst)      
    In
      last (0 - 1)
  ", list_1) ;;
# expand_eval macro_list code_8 ;;
- : Fbast.expr = Int 6
*)

--------------------------------------------------------------------------------------------------
  Operational Equivalence
--------------------------------------------------------------------------------------------------

2. For each of the following Fb expressions, indicate whether
   operational equivalence holds. If it does not, show some context
   that evaluates differently dependent upon which of the two
   expressions you use. (Remember: it only takes one of the infinitely
   many contexts to make the two expressions operationally inequivalent)
   
   [10 points]

   Note: e1 and e2 are arbitrary expressions; x and y are variables.

   a. (Function y -> Not y) False =~ True 
        
        Yes. 
    
    
   b. x + x =~ x + x + x - x
  
        Yes.    
        
    
   c. Function x -> x + y =~ Function y -> y + x
    
        No.
        Let C = def (Function y -> *) 1 2
        C[Function x -> x + y] = (Function y -> (Function x -> x + y)) 1 2 = 3
        C[Function y -> y + x] = (Function y -> (Function y -> y + x)) 1 2 will raise exception, so it can't be evaluated
        Therefore they are not operational equivalance.

                
   d. Function x -> Function y -> x y =~ Function y -> Function x -> y x
  
        Yes. 

        
   e. (Function z -> y) x =~ y
  
        No.
        Let C = def (Function y -> * ) 1
        C[(Function z -> y) x] = (Function y -> ( (Function z -> y) x ) ) 1 will raise exception, so it can't be evaluated
        C[y] = (Function y -> y ) 1 = 1
        Therefore they are not operational equivalance.
        

--------------------------------------------------------------------------------------------------
  Operational Semantics
--------------------------------------------------------------------------------------------------

3a. Write out the operational semantics rules for for FbL, Fb with lists. We define the
    following list syntax (making it a little different from OCaml to avoid confusion)
     - "[[ ]]". Syntax for empty list
     - "e ++ e". This is the cons operator (similar to :: in OCaml and has similar
        semantics. An example element list: "1 ++ (True ++ (3 ++ [[ ]]))"
     - "Match e With [[ ]] -> e | (x ++ x) -> e". This is an FbL specific match operation.
        (Note: x is the metavariable for variables). So for example the following returns 11:
        Match (1 ++ (2 ++ [[ ]])) With [[ ]] -> 0 | (h ++ t) -> h + 10

    For your answer:
      1. Make sure to define any new values in FbL (beyond those present in Fb)
      2. Give the new operational semantics rules that would have to be 
         added to standard Fb rules. 

    [10 Points]
        
        1.  v ::= ... | [[ ]] | [v; ...; v]   
                e ::= ... | [e; ...; e] | e ++ e | (Match e With [[ ]] -> e | (x ++ x) -> e)
        
        2.          
        
        e_1 => v_1  ... e_n => v_n
        ----------------------------------
        [e_1; ...; e_n] => [v_1; ...; v_n]
        
        
        
                    e1 => [v_11; ...; v_1m]  e2 => [v_21; ...; v_2n]
                    -------------------------------------------------
                        e1 ++ e2 => [v_11; ...; v_1m; v_21; ...; v_2n]

                        
                e1 => [[ ]]  e2 => [v_21; ...; v_2n]
                ------------------------------------                                                        
                        e1 ++ e2 => [v_21; ...; v_2n]
                        
                    
                e1 => [v_11; ...; v_1m]   e2 => [[ ]]
                --------------------------------------                                                  
                        e1 ++ e2 => [v_11; ...; v_1m]
                                                                                                
                                                                                                
                e1 => [[ ]]   e2 => [[ ]]
                --------------------------                                                                                                                                                              
                      e1 ++ e2 => [[ ]]                         
                        
                                                                                                                        
            
                             e => [[ ]]   e1 => v
        ----------------------------------------------
        Match e With [[ ]] -> e1 | (h ++ t) -> e2 => v
        
        
            e => [v_1; ...; v_m]   (e2[v_1/h])[[v_2; ...; v_m]/t] => v 
        --------------------------------------------------------------------------- 
                            Match e With [[ ]] -> e1 | (h ++ t) -> e2 => v
                            
                            
                               e => [v_1]   (e2[v_1/h])[( [[ ]] )/t] => v 
        ---------------------------------------------------------------------------
                            Match e With [[ ]] -> e1 | (h ++ t) -> e2 => v



