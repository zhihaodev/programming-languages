(*
    600.426 - Programming Languages
    JHU Spring 2015
    Homework 3 Part 2 (35 points)

    Now that you have completed your Fb interpreter, you will need to write some
    programs in Fb.

    Note: We will be testing your Fb code with the canonical interpreter (binary) that was
    provided to you. So it is worth your while to test your code against that prior to submitting
    it.
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*

  Name                  : Zhihao Cao
  List of team Members  : Hefeng Sun
  List of discussants   :

*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : Operational Semantics and Proofs                                                       *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  1a. The operational semantics for Fb provide a set of proof rules which are used by the interpreter to
      perform evaluation.  In this problem, you will build a proof by hand.  Write an operational semantics
      proof which demonstrates that the Fb expression "(Fun f -> Fun y -> f y) (Fun z -> z + 1) 2"
      evaluates to 3.
      
      Your proof should follow the Fb operational semantics rules correctly and not skip any steps.
  
      [5 points]
*)

(* ANSWER *)

(*
																								                                                            2 => 2, 1 => 1       
																																																				    --------------
(Fun f -> Fun y -> f y) => *, Fun z -> z+1, Fun y -> (Fun z -> z+1) y => *			   Fun z -> z+1 => *, 2 => 2,  2+1 => 3
--------------------------------------------------------------------------         -------------------------------------
   (Fun f -> Fun y -> f y) (Fun z -> z+1) =>  Fun y -> (Fun z -> z+1) y,    2 => 2,      (Fun z -> z+1) 2 => 3
--------------------------------------------------------------------------------------------------------------
                                (Fun f -> Fun y -> f y) (Fun z -> z+1) 2 => 3


*)

(*
  1b. The sequence operation (;) is defined as a macro in the book. But it is also possible to augment Fb
      with a new grammar construction 'e;e' with a similar behavior. Write out the operational semantics for
      this new operation.
      
      [4 Points]
*)

(* ANSWER *)

(*
e1 => v1, e2 => v2
-------------------
	e1; e2 => v2
*)


(*
  1c. In languages like C or Java, binary boolean operators are 'short circuiting' - i.e. For an expression
      like "file != null && file.length() > 0", the second expression is evaluated only if the first expression
      is true. Similarly for '||' operations, the second expression is evaluated only if the first expression
      evaluates to false.

      This is not true in the default Fb operational semantics.

      Modify the operational semantics of Fb such that 'And' and 'Or' operators perform short-circuiting.

      [6 Points]
*)

(* ANSWER *)

(*
And:

					e1 => True, e2 => v2
-------------------------------------------
e1 And e2 => the logical and of True and v2

   e1 => False
------------------
e1 And e2 => False


Or:

					e1 => False, e2 => v2
-------------------------------------------
e1 Or e2 => the logical or of False and v2

   e1 => True
------------------
e1 Or e2 => True

*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : (En)Coding in Fb                                                                      *)
(* -------------------------------------------------------------------------------------------------- *)

(*
    The answers for this section must be in the form of Fb ASTs. You may assume that
    "fbdktoploop.ml" has been loaded before this code is executed; thus, you may use
    the parse function to create your answer if you like. Alternately you can create
    ASTs directly.

    For instance, the two definitions of the identity function in Fb are equivalent. (See below)
    The second one directly declares the datastructure produced by the first expression.

    You may use whichever form you please; the parse form is somewhat more readable, but
    the AST form allows you to create and reuse subtrees by declaring OCaml variables.
    
    For questions in this section you are not allowed to use the Let Rec syntax even if you
    have implemented it in your interpreter. Any recursion that you use must entirely be in 
    terms of Functions. Feel free to implement an Fb Y-combinator here.  For examples and 
    hints, see the file "src/Fb/fbexamples.ml" in the FbDK project.
    
    Remeber to test your code against the standard Fb binaries (and not just your own 
    implementation of Fb) to ensure that your functions work correctly.
*)

let fb_identity_parsed = parse "Function x -> x";;

let fb_identity_ast = Function(Ident("x"), Var(Ident("x")));;

(*
  2a. Fb is such a minimalisitc language that it does not even include a < operation.
      But it is possible to create one of your own.

      Hint: a) You can call upon your powers of recursion ;) b) We dont really care
      about efficiency.

      [5 Points]
*)

let yY = "(Fun f ->(Fun x -> f (Fun n -> (x x) n))(Fun x -> f (Fun n -> (x x) n)))";;

let fbltFun =  "(Fun fs -> Fun c -> Fun a -> Fun b -> If a - b = c Then False Else If b - a = c Then True Else fs (c + 1) a b)";;

let fblt = Appl(parse (yY ^ fbltFun), Int 0);;


(*
# ppeval (Appl(Appl(fblt, Int 2), Int 3)) ;;
==> True
- : unit = ()
# ppeval (Appl(Appl(fblt, Int(-3)), Int(-4))) ;;
==> False
- : unit = ()
*)

(*
  2b. Now write an Fb function that given two positive integers a and b, computes a modulo b
      
      [3 Points]
*)

let fbModFun = "(Fun fs -> Fun fblt -> Fun a -> Fun b -> If (fblt 0 a) Then fs fblt (a - b) b Else If a = 0 Then 0 Else (a + b) )";;

let fbMod = Appl(parse (yY ^ fbModFun), fblt);;

(*
# ppeval (Appl(Appl(fbMod,Int(12)),Int(4)));;
==> 0
- : unit = ()
# ppeval (Appl(Appl(fbMod,Int(7)),Int(3)));;
==> 1
- : unit = ()
# ppeval (Appl(Appl(fbMod,Int(64)),Int(5)));;
==> 4
- : unit = ()
*)

(*
  2c. We can even encode fairly complex data structures For this question we will consider the encoding of a
      simple dictionary/map data type. For simplicity we will assume that they keys and values for this data
      structure are Ints.
      
      The dictionary data structure is defined by the four functions below.
      
      Hint: This is simpler than it looks. For the simplest answer, you do not even require the Y-combinator.
      
      [12 Points]
*)

(* Write a Fb function that takes a single dummy argument as input and returns a new empty dictionary *)

let pr = parse "Fun lft -> Fun rgt -> Fun x -> x lft rgt";;
let fbDictEmpty = Appl(parse "Fun pr -> Fun x -> pr (pr True (pr 0 0) ) x", pr)
;;

(* 
  Write an Fb function that takes a dictionary, a key and a value and returns a new dictionary with the key
  mapped to the specified value.
*)
let left = parse "(Function x -> Function y -> x)";;
let right = parse "(Function x -> Function y -> y)";;

let add = "(Fun fs -> Fun pr -> Fun left -> Fun right -> Fun mark -> Fun dict -> Fun key -> Fun value ->
									If ((dict left) left) And mark Then
										dict
									Else If ((dict left) left) And (Not mark) Then
										pr ( pr False (pr key value) ) dict
									Else If (((dict left) right) left) = key Then
										pr (pr False (pr key value)) (fs pr left right True (dict right) key value)
									Else 
										pr (dict left) ( fs pr left right mark (dict right) key value) )";;

let fbDictAdd  = Appl(Appl(Appl(Appl(parse (yY ^ add), pr), left ), right), Bool false);;


(*  
  Write an Fb function that takes a dictionary and key as input and returns the value mapped to the key if any.
  Fb does not provide a direct way to report errors. So in case the dictionary does not contain the key, your 
  function should diverge. 
*)  
let get = "(Fun fs -> Fun left -> Fun right -> Fun dict -> Fun key ->
									If (((dict left) right) left) = key Then
										((dict left) right) right								
									Else fs left right (dict right) key)";;

let fbDictGet = Appl(Appl(parse (yY ^ get), left), right);;


(* Write an Fb function that takes a dictionary and a key as input and returns a new dictionary such that the key
   is no longer mapped. You are allowed to assume that this function is only called with keys known to be present 
   in the dictionary.
*)  
let remove = "(Fun fs -> Fun pr -> Fun left -> Fun right -> Fun dict -> Fun key ->
									If ((dict left) left) Then
										dict
									Else If (((dict left) right) left) = key Then
										fs pr left right (dict right) key
									Else 
										pr (dict left) ( fs pr left right (dict right) key) )";;

let fbDictRemove = Appl(Appl(Appl(parse (yY ^ remove), pr), left), right);;

(*
# let empty = Appl(fbDictEmpty, Int 0) ;;
# let dict_a = eval ( Appl(Appl(Appl(fbDictAdd, empty), Int 1), Int 11) );;
# let dict_b = eval ( Appl(Appl(Appl(fbDictAdd, dict_a), Int 2), Int 12) ) ;;
# ppeval ( Appl(Appl(fbDictGet, dict_b), Int 1) ) ;;
==> 11
- : unit = ()
# ppeval ( Appl(Appl(fbDictGet, dict_b), Int 2) ) ;;
==> 12
- : unit = ()
# ppeval (Appl(Appl(fbDictGet, Appl(Appl(Appl(fbDictAdd, empty), Int 1), Int 121)), Int 1)) ;;
==> 121
- : unit = ()
# ppeval ( Appl(Appl(fbDictGet, dict_b), Int 10) ) ;;
==> Exception: ...
# let dictCreate keyList valList = List.fold_left2 (
    fun res -> fun k -> fun v -> Appl(Appl(Appl(fbDictAdd, res), k), v)
  ) empty keyList valList ;;
# let dict_c = eval ( dictCreate [Int 4; Int 3; Int 2; Int 1; Int 0] [Int 404; Int 303; Int 202; Int 101; Int 0] ) ;;
# ppeval ( Appl(Appl(fbDictGet, dict_c), Int 0) ) ;;
==> 0
- : unit = ()
# ppeval ( Appl(Appl(fbDictGet, dict_c), Int 3) ) ;;
==> 303
- : unit = ()
# let dict_d = ( Appl(Appl(fbDictRemove, dict_c), Int 3) ) ;;
# ppeval ( Appl(Appl(fbDictGet, dict_d), Int 3) ) ;;
==> Exception: ...
# let dict_e = ( Appl(Appl(fbDictRemove, dict_d), Int 2) ) ;;
# ppeval ( Appl(Appl(fbDictGet, dict_e), Int 2) ) ;;
==> Exception: ...
# let dict_f = ( Appl(Appl(Appl(fbDictAdd, dict_e), Int 3), Int 606) ) ;;
# ppeval ( Appl(Appl(fbDictGet, dict_f), Int 3) ) ;;
==> 606
- : unit = ()
*)