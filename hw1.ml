(*
  600.426 - Programming Languages
  JHU Spring 2015
  Homework 1
  
  In this source file, you will find a number of comments containing the text
  "ANSWER".  Each of these comments indicates a portion of the source code you
  must fill in yourself.  Read the instructions for each problem and supply a
  segment of code which accomplishes the indicated task.  For your convenience,
  a number of test expressions are provided for each problem as well as a
  description of their expected values.
  
  Please also fill in the HEADER section right below this one.
  
  Please note that you are NOT permitted to use any OCaml module functions (such
  as List.length) to complete this assignment unless it is explicitly specified in the 
  question. You are also not permitted to make use of mutation (references, arrays, etc.). 
  On the other hand you are encouraged to write additional helper functions and reuse earlier 
  functions in the file.
  
  Make sure to eliminate compiler/interpreter warnings before submitting your code.
  
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*
 
  Name                        : Zhihao Cao
  List of team Members        : Hefeng Sun
  List of other collaborators :

*) 

(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : A list of list functions                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  1a. To warmup, write a function that takes a list and returns all elements in the list except the last. 
      If the initial list is empty, return empty list as the result. 

      [5 Points]
*)

let rec initial lst = match lst with
                      [] -> []
                    | hd :: [] -> []
                    | hd :: tl -> hd :: initial tl;;

(*
# initial ["Foo"; "Bar"; "Baz"] ;;
- : string list = ["Foo"; "Bar"]
# initial [10;9;8;7;6;5;4;3;2;1;0] ;;
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
*)

(*
  1b. Write a function 'split' that takes a parameter 'n' and a list and returns a pair of lists where the first element
      consists of the first n values from the source list and the second element is the remainder of that list.  If n is
      greater than the length of the list, the first list should contain the entire source list and the second list should
      be empty.
      
      You can assume that n >= 0.
      
      [5 Points]
*)

let rec split n lst = match lst with
                      [] -> ([], [])
                    | hd :: tl -> 
                      let (l1, l2) = split (n - 1) tl in
                      if n > 0 then
                        (hd :: l1, l2)
                      else
                        (l1, hd :: l2)
;;
                    
   
(*
# split 3 [1;2;3;4;5;6;7;8;9] ;;
- : int list * int list = ([1; 2; 3], [4; 5; 6; 7; 8; 9])
# split 3 [1;2] ;;
- : int list * int list = ([1; 2], [])
*)

(*
  1c. Write a function to remove duplicate elements from a list. 
 
      [7 Points]
*)


let rec contains x lst = match lst with
                          [] -> false
                        | hd :: tl ->
                            x = hd || contains x tl
;;

let rec rev lst = 
  match lst with
    [] -> []
  | hd :: tl -> rev tl @ [hd]
;;

let rec remove_duplicates lst = match (rev lst) with
                                [] -> []  
                              | hd :: tl ->
                                let l = remove_duplicates (rev tl) in
                                if not (contains hd l) then
                                  l @ [hd]
                                else
                                  l                               
;;
      
(*
# remove_duplicates [1;3;2;4;3;1;5]  ;;
- : int list = [1; 3; 2; 4; 5]
# remove_duplicates ['A';'Q';'B';'A';'D';'Q']  ;;
- : char list = ['A'; 'Q'; 'B'; 'D']
*)

(*
  1d. Given two lists and a binary function 'f', combine the two in to a single list by "merging"
      values at corresponding locations using f. In other words if lst_1 = [a0 ; a1 .. an] and lst_2 = [b0 ; b1 .. bn],
      write a function that generates the list [ (f a0 b0) ; (f a1 b1) ; ... (f an bn) ]. 
        
      If the input lists are not of the same size, raise an exception. You can use the "invalid_args" function from
      Pervasives for this. 
        
      [5 Points]
*)

let rec combine_with f lst1 lst2 = match (lst1, lst2) with
                                    ([], []) -> []
                                  | ([], _) -> invalid_arg "The input lists have different size!"
                                  | (_, []) -> invalid_arg "The input lists have different size!"
                                  | (hd1 :: tl1, hd2 :: tl2) ->
                                    (f hd1 hd2) :: (combine_with f tl1 tl2)
;;


(*
# combine_with (+) [1; 2; 3] [4 ; 5; 6] ;;
- : int list = [5; 7; 9]
# combine_with (fun x -> fun y -> x *. (float_of_int y)) [1.1; 2.2; 3.4] [2 ; 4; 6] ;;
- : float list = [2.2; 8.8; 20.4]
# combine_with ( fun x -> fun y -> (string_of_int x) ^ "-" ^ (string_of_int y) ) [0xa] [0xc] ;;
- : string list = ["10-12"]
*)


(*
  1e. Many programming languages support the idea of a list comprehension - a mechanism
      to construct new lists from existing lists. The syntax is usually based on
      the mathematical set builder notation.

      For example the python expression lst = [ x*x for x in range(10) if x % 2 = 0 ]
      creates a list whose values are squares of even integers between 0 and 10 (exclusive).

      This construct is very functional in nature; so let us define a function - list_comprehension
      to help us with this. It takes 3 parameters - a source list of values (source), a predicate to
      filter values (pred) and a computation function (compute) to create a new value. The output is
      the list of values produced by the compute function for those source values that satisfy the
      predicate. The order of the items in the output list is based on the order in the source list.

      E.g. list_comprehension [0;1;2;3;4;5;6;7;8;9] (fun x -> (x mod 2) = 0) (fun x -> x * x) produces
      the list [0, 4, 16, 36, 64] similar to the python expression above

      [5 Points]
*)

let rec list_comprehension source pred compute = match source with
                                                  [] -> []
                                                | hd :: tl ->
                                                  if pred hd then
                                                    (compute hd) :: (list_comprehension tl pred compute)
                                                  else
                                                    list_comprehension tl pred compute
;;


(*
# let rec range n = match n with 1 -> [0] | x -> (range (n-1)) @ [x-1] ;;
val range : int -> int list = <fun>
# list_comprehension (range 101) (fun x -> (x mod 5) = 0) (fun x -> x / 5) ;;
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 2: Quick Sort                                                                              *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  2a. First write a partition function that takes a list and a pivot value and returns a triple of lists. 
      The first list should contain all values less than the pivot, the second list contains 
      values equal to the pivot and the third list contains values greater than the pivot.  
  
      It is possible to use the < operator for comparison since it is polymorphic. However we can
      generalize this a bit further by allowing the caller to supply a comparison function. The comparison 
      function is expected to have the type (a -> a -> comparison_result). See test cases below.
  
      [10 Points]
*)

type comparison_result = LT | EQ | GT ;;

let rec partition compare_fn lst pivot = match lst with
                                          [] -> ([], [], [])
                                        | hd :: tl ->
                                          let (llst, elst, glst) = partition compare_fn tl pivot in
                                          if compare_fn hd pivot = LT then
                                            (hd :: llst, elst, glst)
                                          else if compare_fn hd pivot = EQ then
                                            (llst, hd :: elst, glst)
                                          else
                                            (llst, elst, hd :: glst)
;;


(*
  2b. Now write a function to sort a given list using the quicksort algorithm. You take a
      comparison function and a list of data as input.

      [10 Points]
*)

let rec quicksort compare_fn lst = match lst with
                                    [] -> []
                                  | hd :: tl ->
                                    let (llst, elst, glst) = partition compare_fn tl hd in
                                    if elst = [] then
                                      (quicksort compare_fn llst) @ (hd :: quicksort compare_fn glst)
                                    else
                                      (quicksort compare_fn llst) @ (elst @ quicksort compare_fn glst)
;;

let rec quicksort compare_fn lst = match lst with
                                    [] -> []
                                  | hd :: tl ->
                                    let (llst, elst, glst) = partition compare_fn tl hd in
                                      (quicksort compare_fn (llst @ elst)) @ (hd :: quicksort compare_fn glst)
;;

(*
# let simple_compare x y = if (x < y) then LT else (if (x = y) then EQ else GT) ;;
# quicksort simple_compare [3;5;2;9;7;6;4;1]  ;;
- : int list = [1; 2; 3; 4; 5; 6; 7; 9]
# quicksort (fun (a,b) -> fun (c,d) -> simple_compare (a+b) (c+d)) [(1,4);(2,1);(3,4);(5,1);(6,2)] ;;
- : (int * int) list = [(2, 1); (1, 4); (5, 1); (3, 4); (6, 2)]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 3 : Numerical Methods                                                                      *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  3a. A discrete function 'f' can be represented as a list of pairs [(x_1, v_1); (x_2, v_2); ... (x_m, f(x_m)] 
      such that v_n = f(x_n) and x_m > ... > x_2 > x_1. With such a representation, one approximation for the 
      derivative at (say) x_1 is to compute d(x_1) = f(x_2) - f(x_1) / (x_2 - x_1)
      
      Given a list like the above (with values represented as floating point numbers), write a function that 
      returns the list [d(x_1); d(x_2); ... ]. 
      
      Note that since we are computing forward differences, the last entry in the list will not have a 
      corresponding deriviative. (In other words a list of length n should return a derivative list of size n-1)

      Also note that due to the inaccuracy of floating point representations, the computed answers may differ 
      *slightly* from what you compute by hand.
      
      [10 Points] 
*)

let diff x1 x2 = let (a1, b1) = x1 in
                  let (a2, b2) = x2 in
                    (b2 -. b1) /. (a2 -. a1)
;;  

let rec differentiate lst = match lst with
                            [] -> []
                          | hd :: [] -> []
                          | hd1 :: hd2 :: tl -> 
                            diff hd1 hd2 :: differentiate (hd2 :: tl) 
;;                        


(*
# differentiate [(0.1,5.0); (0.2,7.0); (0.3,6.0); (0.4,4.0); (0.5,1.0)] ;;
- : float list = [20.; -10.0000000000000018; -19.9999999999999929; -30.0000000000000071]
*) 

(*
  3b. A direct, if not necessarily the best, way to approximate the integral of a function like the above is 
      to use the trapezoidal rule - https://en.wikipedia.org/wiki/Trapezoidal_rule
      
      The basic idea is as follows: if (x_a, v_a) and (x_b, v_b) are adjacent entries in the list consider 
      the trapezium formed by the points (x_a, 0) (x_b, 0), (x_a, v_a), (x_b, v_b), compute its area as: 
      area_a = 0.5 * (x_b - x_a) * (v_b + v_a); the final value is the sum of all the trapeziums traced
      by the function. For a list of length n, there are n-1 trapeziums to consider.
      
      [10 Points]
*)

let area x1 x2 = let (a1, b1) = x1 in
                  let (a2, b2) = x2 in
                    0.5 *. (a2 -. a1) *. (b2 +. b1)
;;  

let rec integrate lst = match lst with
                        [] -> 0.
                      | hd :: [] -> 0.
                      | hd1 :: hd2 :: tl ->
                        area hd1 hd2 +. integrate (hd2 :: tl)
;;


(* 
# integrate [(-3.0,81.0); (-2.0,16.0); (-1.0,1.0); (0.0,0.0); (1.0,1.0); (2.0,16.0); (3.0,81.0)] ;;
- : float = 115.
# integrate 
     [(0.0, 0.0); (0.125, 0.001953125); (0.25, 0.015625); (0.375, 0.052734375);
      (0.5, 0.125); (0.625, 0.244140625); (0.75, 0.421875); (0.875, 0.669921875);(1.0, 1.0)] ;;
- : float = 0.25390625
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 4 : Sudoku Verifier                                                                       *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  For this question we will build a simple sudoku checker. The final goal is to write a function that
  when presented with a sudoku grid, returns whether it has been solved or not. In case you are not familiar
  with the puzzle, you can read about it here: https://en.wikipedia.org/wiki/Sudoku. 
  
  A filled grid looks like this (courtesy wikipedia): 
  https://en.wikipedia.org/wiki/Sudoku#mediaviewer/File:Sudoku-by-L2G-20050714_solution.svg
  
  To verify a grid, we have to check that each row, each column and each of the (bolded) 3x3 regions are 
  some permutation of the sequence 1, 2, ..., 9. We will build the machinery for this piece-by-piece
  
  A straight forward way to represent a sudoku grid is as a list of 9 rows, each of which is also a list of
  9 integers.
  
  Here are a few grids for testing purposes:
  
  # let solved_grid_1 = [[5;3;4;6;7;8;9;1;2]; [6;7;2;1;9;5;3;4;8]; [1;9;8;3;4;2;5;6;7];
                         [8;5;9;7;6;1;4;2;3]; [4;2;6;8;5;3;7;9;1]; [7;1;3;9;2;4;8;5;6];
                         [9;6;1;5;3;7;2;8;4]; [2;8;7;4;1;9;6;3;5]; [3;4;5;2;8;6;1;7;9]] ;;

  # let solved_grid_2 = [[2;4;6;8;5;7;9;1;3]; [1;8;9;6;4;3;2;7;5]; [5;7;3;2;9;1;4;8;6];
                         [4;1;8;3;2;9;5;6;7]; [6;3;7;4;8;5;1;2;9]; [9;5;2;1;7;6;3;4;8];
                         [7;6;4;5;3;2;8;9;1]; [3;2;1;9;6;8;7;5;4]; [8;9;5;7;1;4;6;3;2]] ;;

  # let unsolved_grid = [[1;9;4;2;8;6;7;3;5]; [8;3;7;5;9;4;2;1;6]; [6;2;5;3;5;7;8;9;4];
                         [7;4;3;9;4;8;1;6;2]; [2;5;1;6;5;3;9;4;8]; [9;8;6;1;7;2;3;5;7];
                         [5;6;2;8;3;1;4;7;9]; [4;1;9;7;2;5;6;8;3]; [3;7;8;4;6;9;5;2;1]] ;;

  Hint: You have already written many functions in other sections that can come in handy here. Also be sure to glance
        at the lecture notes for even more useful functions.
*)

(*
  4a. With the list-of-lists arrangement, it is easy to access rows from the grid. Columns are a bit harder. For
      this question, write a function that when given a column index (zero-based) and a grid, extracts the specific 
      column. 
      
      [8 Points]
*)

exception Failure;;

let rec nth l n = match l with
                  [] -> raise Failure
                | hd :: tl -> 
                  if n = 0 then 
                    hd
                  else
                    nth tl (n - 1)
;;

let rec fetch_column grid col = match grid with
                                [] -> []
                              | hd :: tl ->
                                nth hd col :: fetch_column tl col
;;

(*
# fetch_column solved_grid_1 4 ;;
- : int list = [7; 9; 4; 6; 5; 2; 3; 1; 8]
# fetch_column solved_grid_2 7 ;;
- : int list = [1; 7; 8; 6; 2; 4; 9; 5; 3]
*)

(*
  4b. Now write a function, given a starting row, starting column, a row count and column count, extracts the
      specified subsection of grid as a single list in row-major order. 

      [10 Points]
*)

let rec extract a b lst = match lst with
                      [] -> []
                    | hd :: tl -> 
                      let l = extract (a - 1) (b - 1) tl in
                      if a <= 0 && b > 0  then
                        hd :: l
                      else
                        l
;;  

let rec fetch_grid grid start_row start_col row_count col_count =
  if (start_row + row_count) > 9 || (start_col + col_count) > 9 then
    raise Failure
  else
    match grid with
      [] -> []
    | hd :: tl ->
      let l = fetch_grid tl (start_row - 1) start_col row_count col_count in
      if start_row <= 0 && (start_row + row_count) > 0 then
        extract start_col (start_col + col_count) hd @ l
      else
        l
;;
    
(*
# fetch_grid solved_grid_1 3 6 3 3  ;;
- : int list = [4; 2; 3; 7; 9; 1; 8; 5; 6]
# fetch_grid solved_grid_2 6 0 3 3 ;;
- : int list = [7; 6; 4; 3; 2; 1; 8; 9; 5]
*)

(*
  4c. Write a predicate function that, given a list, verifies that it is some permutation of the sequence:
      1, 2, ..., 9 
      
      [5 Points]
*)

let rec verify n lst = match lst with
                        [] ->
                          if (n = 10) then 
                            true
                          else
                            false
                      | hd :: tl ->
                          hd = n && verify (n + 1) tl
;; 

let verify_list lst = verify 1 (quicksort (simple_compare) lst);;

(*
# verify_list [4; 2; 3; 7; 9; 1; 8; 5; 6] ;;
- : bool = true
# verify_list [8; 9; 5; 4; 5; 7; 3; 2; 6] ;;
- : bool = false
*)

(*
  4d. Finally write a function that checks whether a given sudoku grid has been solved or not
  
      [10 Points]
*)

let rec checkRows grid = match grid with
                          [] -> true
                        | hd :: tl -> verify_list hd && checkRows tl
;;

let rec checkCols col grid = match col with
                              9 -> true
                            | _ -> verify_list (fetch_column grid col) && checkCols (col + 1) grid
;;

let rec checkRegions start_row grid = match start_row with
                                                9 -> true
                                              | _ ->
                                                verify_list (fetch_grid grid start_row 0 3 3) &&
                                                verify_list (fetch_grid grid start_row 3 3 3) &&
                                                verify_list (fetch_grid grid start_row 6 3 3) &&
                                                checkRegions (start_row + 3) grid
;;

let verify_grid grid = checkRows grid && checkCols 0 grid && checkRegions 0 grid;;

(*
# verify_grid solved_grid_1 ;;
- : bool = true
# verify_grid solved_grid_2 ;;
- : bool = true
# verify_grid unsolved_grid ;;
- : bool = false
*)
