(*
  600.426 - Programming Languages
  JHU Spring 2015
  Homework 2

  In this source file, you will find a number of comments containing the text
  "ANSWER".  Each of these comments indicates a portion of the source code you
  must fill in yourself.  You are welcome to include other functions for use in
  your answers.  Read the instructions for each problem and supply a segment of
  code which accomplishes the indicated task.  For your convenience, a number of
  test expressions are provided for each problem as well as a description of
  their expected values.

  In this assignment, you *are* permitted to complete the listed tasks using any
  of the OCaml modules/functions.  However you are still required to avoid the use 
  of mutation unless explicitly specified in the question.
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*
 
  Name                        : Zhihao Cao
  List of team Members        : Hefeng Sun
  List of other collaborators :

*) 

(* ----------------------------------------------------------------------------------------- *)
(* Section 1 : The Game of Types                                                             *)
(* ----------------------------------------------------------------------------------------- *)

(*
  1. For the next several problems, you will be asked to produce an expression which
     has a given type.  It does not matter what expression you provide as long as it
     has that type; there may be numerous (or even infinite) answers for each
     question. Your answer may *not* produce a compiler warning.  You are *not*
     permitted to use explicit type annotations (such as "fun x:'a -> x"). You *are*
     allowed to use mutable state and exceptions where necessary.

     [20 Points]
*) 

(* Give an expression which has the following type: int list ref *)
let exp1 = ref [4; 4];;


(* Give an expression which has the following type: 'a -> ('a -> 'b) -> 'b *)
let exp2 = fun x -> fun f -> f x;;


(* Give an expression which has the following type: unit -> unit *)
let exp3 = fun () -> ();;


(* Give an expression which has the following type: int -> 'a list -> 'a *)
exception Failure;;

let exp4 = fun n -> fun lst -> match lst with
                  [] -> raise Failure
                | hd :: tl -> 
                  if n = 0 then 
                    hd
                  else
                    hd
;; 


(* Give an expression which has the following type: 'a list list -> 'a list *)
let exp5 = fun lst -> match lst with  
                      [] -> []
                    | hd :: tl -> hd
;;


(* Give an expression which has the following type: ('a -> 'b) -> ('a -> 'c) -> 'a list -> ('b * 'c) list *)
let rec exp6 = fun f g lst -> match lst with
                          [] -> []
                        | hd :: tl -> 
                          (f hd, g hd) :: exp6 f g tl
;;


(* Give an expression which has the following type: 'a list -> 'b list -> ('a -> 'b -> unit) -> unit *)
let exp7 = fun lst1 lst2 f -> match (lst1, lst2) with
                            ([], _) -> ()
                          | (_, []) -> ()
                          | (hd1 :: tl1, hd2 :: tl2) ->
                              f hd1 hd2 
;;


(* Give an expression which has the following type: 'a -> 'b  *)
(* Hint: Trick question. Try staring at methods in Pervasives for a bit *)
let exp8 = fun x -> raise Failure;;


(* Give an expression which has the following type: ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option *)
let exp9 =  fun f g x -> match (f x) with
                          None -> None
                        | Some hd -> g hd
;;
                     

type ('a, 'b) foobar = Foo of 'a | Bar of 'b ;;

(* Give an expression which has the following type:
   ('a, 'b) foobar list -> ('b -> bool) -> ('c, 'b) foobar list
*)
let exp10 = fun lst f -> match lst with 
                            [] -> []
                          | Foo hd :: tl -> exp8 hd :: []
                          | Bar hd :: tl -> if f hd then 
                                              Bar hd :: []
                                            else
                                              raise Failure                                                                                                                 
;;


(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : Making Modules, Using Modules                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  2a. Create a simple stack class that has the following methods:
      * emptystack : unit -> 'a stack         - The method returns a *new* empty stack instance
      * push : 'a -> 'a stack -> 'a stack     - Push a new value on to the stack and return the new stack 
      * top : 'a stack -> 'a option           - Return the value at the top of the stack if there is one.
      * pop : 'a stack -> 'a stack            - Pop the top value off the stack and return the new stack. If the 
                                                stack is empty, throw an exception (use invalid_arg)
      * is_empty : 'a stack -> bool           - Returns true if the stack is empty, otherwise false
    
      In the section below, fill out the signature and the implementation details. You must explicitly leave any
      types in the signature abstract. (This is good practice in the software engineering sense. By not explicitly
      binding the types on the interface, you allow different implementations to choose types best suited
      for their goals)
    
      NOTE: When you query the type of your functions in the top loop, it might return the fully qualified type signatures.
      E.g: emptystack : unit -> 'a GStack.stack instead of just unit -> 'a stack. This is fine. 
        
      [10 Points]
*)

module type GSTACKTYPE =
  sig
    type 'a stack  
    val emptystack : unit -> 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val top : 'a stack -> 'a option
    val pop : 'a stack -> 'a stack    
    val is_empty : 'a stack -> bool                            
  end
;;


module GStack : GSTACKTYPE =
  struct
    type 'a stack = 'a list
    
    let emptystack () : 'a stack = []
    
    let push x stack = x :: stack
    
    let top stack = match stack with
                      [] -> None
                    | hd :: tl -> Some hd
                  
    let pop stack = match stack with
                      [] -> invalid_arg "Empty stack" 
                    | hd :: tl -> tl
                 
    let is_empty stack = match stack with
                          [] -> true
                        | _ -> false
  end
;;

 
(*
# let s = GStack.emptystack () ;;
val s : '_a GStack.stack = <abstr>
# GStack.is_empty s ;;
- : bool = true
# let s = GStack.push 1 s ;;
val s : int GStack.stack = <abstr>
# GStack.top s;;
- : int option = Some 1
# let s = GStack.pop s ;;
val s : int GStack.stack = <abstr>
# GStack.is_empty s ;;
- : bool = true

# let s = GStack.emptystack () ;;
val s : '_a GStack.stack = <abstr>
# let s = GStack.push "Foo" s ;;
val s : string GStack.stack = <abstr>
# let s = GStack.pop s ;;
val s : string GStack.stack = <abstr>
# let s = GStack.pop s ;;
Exception: Invalid_argument "Empty stack".
*)


(*
  2b. Write a simple postfix calculator that operates over integers and floats and supports
      the mathematical operations +, -, * and /.
      
      Numbers are defined as the type "Int of int | Float of float"
      
      The input is expressed as a list of tokens. The numeric and token types are defined below. 
      
      The output should be a number type. When floats and integers occur in the same expression, the result
      should be a float type. i.e. Mul(Float 3.2, Int 2) evaluates to Float(6.4)
      
      If the input is ill-formed, or the computations result in divide by zero error, raise an exception 
      using invalid_arg.
      
      HINT: It is worth considering the design of this function carefully - What are the sub-operations that 
      you want to perform? Can the repetitive code be abstracted away in to functions?. A few well-written 
      utility functions would serve to simplify the problem quite a lot.
            
      [10 Points]
*)

type number = Int of int | Float of float ;;

type token = Number of number | Plus | Minus | Mul | Div ;;



let rec rev l = 
  match l with
    [] -> []
  | x :: xs -> rev xs @ [x]
;;

let calculate a b op = match ((a : number), (b : number)) with
                        (Int a1, Int b1) -> 
                          (match op with
                            Plus -> Int (a1 + b1)
                          | Minus -> Int (a1 - b1)
                          | Mul -> Int (a1 * b1)
                          | Div -> Int (a1 / b1)
                          | _ -> invalid_arg "Invalid operator" )
                          
                      | (Int a2, Float b2) ->
                          (match op with
                            Plus -> Float (float a2 +. b2)
                          | Minus -> Float (float a2 -. b2)
                          | Mul -> Float (float a2 *. b2)
                          | Div -> Float (float a2 /. b2)
                          | _ -> invalid_arg "Invalid operator")
                      | (Float a3, Int b3) ->
                          (match op with
                            Plus -> Float (a3 +. float b3)
                          | Minus -> Float (a3 -. float b3)
                          | Mul -> Float (a3 *. float b3)
                          | Div -> Float (a3 /. float b3)
                          | _ -> invalid_arg "Invalid operator")
                      | (Float a4, Float b4) ->
                          (match op with
                            Plus -> Float (a4 +. b4)
                          | Minus -> Float (a4 -. b4)
                          | Mul -> Float (a4 *. b4)
                          | Div -> Float (a4 /. b4) 
                          | _ -> invalid_arg "Invalid operator")            
;;


let operation stack op = let x = GStack.top stack in
                              let y = GStack.top (GStack.pop stack) in
                              (match (x, y) with
                                (Some a, Some b) -> calculate b a op
                                | _ -> invalid_arg "Empty stack" )
;;                              
                                
let rec postfix_calculator_stack tokenlist = match (rev tokenlist) with
                                              [] -> GStack.emptystack ()
                                            | hd :: tl ->   
                                              (let s = postfix_calculator_stack (rev tl) in
                                                (match hd with
                                                  Number n -> GStack.push (n: number) s
                                                  | op -> GStack.push (operation s op) (GStack.pop (GStack.pop s))))                        
;;



let postfix_calculator tokenlist = match GStack.top (postfix_calculator_stack tokenlist) with
                                    Some x -> x
                                  | _ ->  invalid_arg "Error"
;;


(*
# postfix_calculator [ Number (Int 3) ; Number (Int 5) ; Minus ] ;;
- : number = Int (-2)
# postfix_calculator [ 
  Number (Int 2); Number (Int 3); Number (Int 8); 
  Mul; Plus; Number (Int 4); Number (Int 48);
  Number (Int 4); Number (Int 2); Plus; Div;
  Number (Int 6); Mul; Plus; Minus
] ;;
- : number = Int (-26)
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 3 : Tries                                                                                  *)
(* -------------------------------------------------------------------------------------------------- *)

(* 
  One reasonably efficient way to store sets of strings or maps with string keys is by using a trie data
  structure. A trie is a tree structure whose edges are labeled with the elements of the alphabet. Each
  node in the tree corresponds to the sequence of alphabets traversed on the path from the root to that 
  node. When used a map, nodes that correspond to keys of the map store an additional value.
             
  For example, this picture: https://en.wikipedia.org/wiki/File:Trie_example.svg from wikipedia represents a map: 
  { to => 7, tea => 3, ted => 4, ten => 12, A => 15, i => 11, in => 5, inn => 9 }.
  
  A couple of points to note:
    a. A typical trie does not actually store the full key at each node as the picture appears to show. This is
       not needed since the path to the node encodes the key.
    b. The "inner" nodes (node "t" and node "te") are not part of the map since they do not have values associated
       with them.
*)

(*
  We will use the following data type for a trie. 
  
*)

type 'a trie = Node of 'a option * (char * 'a trie) list ;; 

(*
  In the above definition, the type variable 'a represents the value stored at a node (which is optional). Thus a 
  node of the trie consists of an optional value and a list of (character - subtrie pairs).
*)

(*
  The ocaml output for tries you construct is generally readable. But for when it is not, here is a pretty printer
  of sorts:
  
  let print_tree value_converter (Node(v, lst)) = 
    let rec print_node_impl (c, (Node(v, lst))) indent = 
      Printf.printf "%s%c => %s\n" indent c (match v with None -> "None" | Some v -> value_converter v) ;
      List.iter (fun v -> print_node_impl v (indent ^ "  ")) lst
   in
    print_string "ROOT\n" ;
    List.iter (fun n -> print_node_impl n "  ") lst
    
  To call this function, you must supply a converter function that can map the values stored in a node to a string.
  For a trie with integer values, you call it like so: 
    print_tree string_of_int the_trie_you_want_to_print
*)

(*
  3a. Given a trie, a string key and a value, write a function that returns a new trie that contains the key value pair.
  
      [10 Points] 

*)

let rec update_node str value = match String.length str with
                                0 -> Node( Some(value),[])  
                              | _ -> Node( None, [String.get str 0, update_node (String.sub str 1 (String.length str - 1))  value] ) 
;;

let rec add_to_trie trie str value = match trie with
                                      Node(v, []) -> 
                                        if String.length str = 0 then 
                                          Node(Some(value), [])
                                        else
                                          Node(v, [(String.get str 0, update_node (String.sub str 1 ((String.length str) - 1)) value)])
                                    | Node(v, lst) -> 
                                        if String.length str = 0 then
                                          Node(Some(value), lst)
                                        else
                                          Node(v, update_list lst str value)
                                        
and 

update_list lst str value = match lst with 
                            [] -> [ (String.get str 0, update_node (String.sub str 1 ((String.length str) - 1)) value) ]
                          | (c, l) :: tl ->                           
                              if c = (String.get str 0) then                                                                                                                                                                                                                
                                (c, add_to_trie l (String.sub str 1 ((String.length str) - 1)) value) :: tl
                              else
                                (c, l) :: (update_list tl str value)
;;

  
(*  
# let root = Node(None, []) ;;  
# let trie_1_n1 = add_to_trie (Node(None, [])) "to" 7 ;;
val trie_1_n1 : int trie =
  Node (None, [('t', Node (None, [('o', Node (Some 7, []))]))])
# let trie_1_n2 = add_to_trie trie_1_n1 "tea" 3 ;;
val trie_1_n2 : int trie =
  Node (None,
   [('t',
     Node (None,
      [('o', Node (Some 7, []));
       ('e', Node (None, [('a', Node (Some 3, []))]))]))])
# let trie_1 = List.fold_left (fun trie -> fun (str, v) -> add_to_trie trie str v) root
  [("to", 7); ("tea", 3); ("ted", 4); ("ten", 12); ("A", 15); ("i", 11); ("in", 5); ("inn", 9)] ;;
val trie_1 : int trie =
  Node (None,
   [('t',
     Node (None,
      [('o', Node (Some 7, []));
       ('e',
        Node (None,
         [('a', Node (Some 3, [])); ('d', Node (Some 4, []));
          ('n', Node (Some 12, []))]))]));
    ('A', Node (Some 15, []));
    ('i', Node (Some 11, [('n', Node (Some 5, [('n', Node (Some 9, []))]))]))])
*)

(*
  3b. Given a trie and a key, fetch the value corresponding to the key if it exists. The return value should
      be an option type. If the key does not exist in the map, return None.
      
      [10 Points]
*)

let rec get_value_from_trie trie key = match trie with
                                        Node(value, lst) ->
                                            if (String.length key) = 0 then
                                              value
                                            else if List.mem_assoc (String.get key 0) lst then
                                              get_value_from_trie (List.assoc (String.get key 0) lst ) (String.sub key 1 ((String.length key) - 1))
                                            else
                                              None
;;                                          
      
                                                              
(*
# get_value_from_trie trie_1 "tea" ;;
- : int option = Some 3
# get_value_from_trie trie_1 "inn" ;;
- : int option = Some 9
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 4 : Trees and Directories                                                                  *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  Most file systems offer a classic recursive view of its contents via directories and files. We can 
  define a simplistic file system type as follows:
*)

type filesystemobject = Directory of string * filesystemobject list | File of string * int ;;

(* In the above definition the string will be the name of the object and int the size of the file *)

(*
  4a. Like most regular file systems, let us assume that the name of a filesystemobject is unique within 
      its parent's namespace. This allows us to define unique paths to each object using just the names 
      and a separator. For our purposes we will fix the separator to be the character '/' and assume that 
      this character cannot occur in object names.
  
      All intermediate parts of the path string are guaranteed to be a directory references. The final part
      can refer to a file or a directory object. i.e. For a path string "foo/bar/moo", the names "foo" and
      "bar" are references to directories. The name "moo" can refer to a directory or a file. For 
      simplicity paths never start or end with a separator.
  
      Given the root filesystemobject (guaranteed to be a directory) and a path string, write a function 
      to return the filesystemobject corresponding to the path string. You can assume that the path 
      provided is always relative to the given root.
  
      If an object corresponding to the path is not found, raise an exception via invalid_arg 
  
      [10 Points]
*)

let rec check_name fso path = match fso with
                              | Directory(name, lst) ->
                                  if not (String.contains path '/') then
                                    name = path
                                  else
                                    name = String.sub path 0 (String.index path '/')
                              | File(name, lst) ->
                                  if not (String.contains path '/') then
                                    name = path
                                  else
                                    name = String.sub path 0 (String.index path '/')
;;

let rec is_name_exist lst path = match lst with
                                  [] -> false
                                | hd :: tl ->  
                                    check_name hd path || is_name_exist tl path
;;
                                                                                                                  
let cut_path name path =if name = String.sub path 0 (String.index path '/') then
                          String.sub path (String.index path '/' + 1) (String.length path - String.index path '/' - 1)
                        else
                          invalid_arg "Error"
;;

let rec search_name lst path = match lst with
                            [] -> invalid_arg "Not found"
                          | hd :: tl ->
                              if check_name hd path then
                                hd
                              else
                                search_name tl path
;;

let rec get_filesystemobject root path = match root with
                                        Directory(name, lst) -> 
                                          
                                          if name = "<ROOT>" then
                                            get_filesystemobject (search_name lst path) path
                                          else if check_name root path && not (String.contains path '/') then
                                            root
                                          else if is_name_exist lst (cut_path name path) then
                                            get_filesystemobject (search_name lst (cut_path name path)) (cut_path name path)
                                          else
                                            invalid_arg "Not found"
                                                    
                                      | File(name, size) ->
                                          if check_name root path && not (String.contains path '/') then
                                            root  
                                          else
                                            invalid_arg "Not found"                                                         
;;  
                                               
 
(*
# let filesystem = 
      Directory("<ROOT>", [ (* The name doesnt really matter for the root *)
          Directory("Apps", [
          Directory("Notes", [
          File("Note.txt", 550) ;
          Directory("600.446", [File("Assignment-2.txt", 150)])
        ]);  
          Directory("Ode", [File("Ode.exe", 1200); File("Ode.txt", 200)]);
          File("TODO.txt", 75);
          File("README", 125)
        ]) ;
          Directory("Config", [
          File(".vimrc", 250);
          File(".bashrc", 250);
        ]) ;
          Directory("Notes", [
          File("Note1.txt", 100);
          File("Note2.txt", 120);
        ])
      ])
    ;;

# get_filesystemobject filesystem "Apps/Ode/Ode.exe" ;;
- : filesystemobject = File ("Ode.exe", 1200)
# get_filesystemobject filesystem "Apps/Notes" ;;
- : filesystemobject =
Directory ("Notes",
 [File ("Note.txt", 550);
  Directory ("600.446", [File ("Assignment-2.txt", 150)])])
# get_filesystemobject filesystem "Apps/Notes/600.446/Assignment-2.txt" ;;
- : filesystemobject = File ("Assignment-2.txt", 150)
# get_filesystemobject filesystem "Config/Apache" ;;
Exception: Not_found.
*)

(*
  4b. Write a function to do the reverse. i.e. Given a filesystemobject and the root
      return the relative path to the object.
  
      [10 Points]
*)

let get_name fobj = match fobj with
                      Directory(name, lst) -> name
                    | File(name, size) -> name
;;

let add_prefix fobj prefix =  if prefix = "<ROOT>" then
                                fobj
                              else 
                                match fobj with
                                  Directory(name, lst) -> Directory(String.concat "/" [prefix; name], lst)
                                | File(name, size) -> File(String.concat "/" [prefix; name], size)
;;
                                                                  
let rec push_list_to_stack stack lst prefix = match lst with
                                                [] -> stack   
                                              | hd :: tl -> 
                                                  let s = push_list_to_stack stack tl prefix in                                   
                                                    GStack.push (add_prefix hd prefix) s
;;  
              

let convert_option x = match x with
                        None -> invalid_arg "Error"
                      | Some n -> n     
;;                                                                      

let compare path name = if not (String.contains path '/') then
                            path = name
                        else
                          let i = String.rindex path '/' in
                            String.sub path (i + 1) (String.length path - i - 1) = name
;;                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                              
let rec get_path_stack fobj stack = if GStack.is_empty stack then
                                      invalid_arg "Not found"
                                    else
                                      let node = convert_option (GStack.top stack) in
                                        if compare (get_name node) (get_name fobj) then
                                          get_name node
                                        else
                                          
                                          match node with
                                            Directory(name, lst) ->
                                              get_path_stack fobj (push_list_to_stack (GStack.pop stack) lst (get_name node)) 
                                          | File(name, size) ->
                                              get_path_stack fobj (GStack.pop stack)
;;    

let get_path fobj root = get_path_stack fobj (GStack.push root (GStack.emptystack ()) ) ;;                                                        
                          
                                                                      

(*
(* -- Wherein we test whether the universe is circular -- *)

# get_path (get_filesystemobject filesystem "Apps/Ode/Ode.exe") filesystem ;;
- : string = "Apps/Ode/Ode.exe"
# get_path (get_filesystemobject filesystem "Apps/Notes") filesystem ;;
- : string = "Apps/Notes"
# get_path (get_filesystemobject filesystem "Apps/Notes/600.446/Assignment-2.txt") filesystem ;;
- : string = "Apps/Notes/600.446/Assignment-2.txt"
*)
              
(*
  4c. A common operation on trees (including directory trees) is to walk the structure 
      in some order and build up a return value. For example, to compute the total size 
      of files in a directory and its sub-directories, one way would be to walk the tree
      in a bottom-up fashion, computing sizes as we go.
  
      This is such a general concept that it is useful to write a function to help us
      with this.
  
      Given a function f with signature ('a -> filesystemobject -> 'a) and an initial
      value of type 'a, write a function that walks the directory structure in postorder 
      (i.e. Child nodes are processed in order and before parent nodes) and builds a result. 
      Note that your function should only 'visit' directories in the tree and not the files.
      
      [5 Points]
*)

let rec visit_list lst = match lst with
                          [] -> []
                        | hd :: tl -> 
                            match hd with
                              Directory(name, lst2) -> (visit_list lst2) @ [hd] @ (visit_list tl) 
                            | File(name, size) -> visit_list tl
;;

let vist_node fobj = match fobj with 
                      Directory(name, lst) -> (visit_list lst) @ [fobj]
                    | File(name, size) -> []
;;

let rec walk_nodes f init lst = match lst with
                          [] -> init
                        | hd :: tl -> walk_nodes f (f init hd) tl
;;

let rec process_filesystem_in_postorder f init root = walk_nodes f init (vist_node root);;


(*
# let filesizesum filelist = List.fold_left (
    fun res -> fun n -> match n with File(name, size) -> res + size | Directory(_, _) -> res
  ) 0 filelist
  ;;
# process_filesystem_in_postorder (
    fun res -> fun n -> match n with 
     | File(_, _)                -> failwith "The processing function should not have been called with a file object"
     | Directory(name, filelist) -> res + (filesizesum filelist)
  ) 0 filesystem
  ;;
- : int = 3020
# module DirSizeMap = Map.Make (struct type t = filesystemobject let compare = Pervasives.compare end) ;;
# let filesizesum_with_map map filelist = List.fold_left (
    fun res -> fun n -> match n with 
      | File(name, size)        -> res + size 
      | Directory(name, _) as d -> res + (DirSizeMap.find d map)
  ) 0 filelist
  ;; 
# let dirsizes = process_filesystem_in_postorder (
    fun dirsizemap -> fun n -> match n with 
     | File(_, _)                     -> failwith "The processing function should not have been called with a file object"
     | Directory(name, filelist) as d -> DirSizeMap.add d (filesizesum_with_map dirsizemap filelist) dirsizemap
  ) DirSizeMap.empty filesystem
  ;;
# DirSizeMap.iter (fun k -> fun v -> match k with 
    | Directory(name, _) -> Printf.printf "%s - %d\n" name v
    | File(_, _)         -> ()
  ) dirsizes
  ;;
600.446 - 150
<ROOT> - 3020
Apps - 2300
Config - 500
Notes - 700
Notes - 220
Ode - 1400
- : unit = ()
*)

(*
  4d. Write a function to find files and directories that match a predicate. If the function
      returns true for a file or directory, it is included in the result. If it returns false
      the file or directory is not included. (Note: If a directory is not included, all its 
      subdirectories are also not included. The entire subtree is pruned)
  
      The result is returned as a new filesystemobject.
  
      Note: The root of the file system cannot go away. Start the filtering operation from
      the children of the provided root. 
  
      [5 Points]
*) 

let rec update_list predicate lst = match lst with
                                    [] -> []
                                  | hd :: tl ->
                                    let newhd = update_node2 predicate hd in
                                      match newhd with

                                        Directory("", _) -> update_list predicate tl
                                      | Directory(_, _) -> newhd :: (update_list predicate tl)
                                      | File(_, 0) -> update_list predicate tl
                                      | File(_, _) -> newhd :: (update_list predicate tl)
                                
  and

  update_node2 predicate root = match root with
                                      Directory(name, lst) ->
                                        if predicate root then
                                          Directory(name, (update_list predicate lst))  
                                        else
                                          Directory("", [])
                                    | File(name, size) ->
                                        if predicate root then
                                          root
                                        else
                                          File(name, 0)                                                                                                                                               
  ;;

let find_files predicate root = match root with
                                  Directory(name, lst) -> Directory(name, update_list predicate lst)
                                | File(_, _) -> root
;;


(*
-- Only unix hidden files --
          
# find_files (
  fun fobj -> match fobj with File(n, _) -> (String.get n 0) = '.' | Directory(_,_) -> true
) filesystem
;;
- : filesystemobject =
Directory (u"<ROOT>",
 [Directory (u"Apps",
   [Directory (u"Notes", [Directory (u"600.446", [])]);
    Directory (u"Ode", [])]);
  Directory (u"Config", [File (u".vimrc", 250); File (u".bashrc", 250)]);
  Directory (u"Notes", [])])
  
-- Everything except the Notes directory *directly under* the root is removed --  
 # find_files (
  fun fobj -> match fobj with File(n, _) -> true | Directory(n,_) -> n = "Notes"
) filesystem
;;
- : filesystemobject =
Directory ("<ROOT>",
 [Directory ("Notes", [File ("Note1.txt", 100); File ("Note2.txt", 120)])])
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 5 : Mutable State and Memoization                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(* Note: You will need to use mutable state in some form for questions in this section *)

(*
  5. Cache: Pure functions (those without side effects) always produces the same value
     when invoked with the same parameter. So instead of recomputing values each time,
     it is possible to cache the results to achieve some speedup.
     
     The general idea is to store the previous arguments the function was called
     on and its results. On a subsequent call if the same argument is passed, 
     the function is not invoked - instead, the result in the cache is immediately 
     returned.  
  
     [10 Points]
*)

(*
  Given any function f as an argument, create a function that returns a
  data structure consisting of f and its cache
*)  
let new_cached_fun f = (f, ref []);;


(*
  Write a function that takes the above function-cache data structure,
  applies an argument to it (using the cache if possible) and returns
  the result 
*)

let rec contains x lst = match lst with
                        [] -> []
                      | (f, _) :: tl -> 
                        if f = x then 
                          lst 
                        else 
                          contains x tl                     
;;

let apply_fun_with_cache cached_fn x = let (f, lst) = cached_fn in
                                          match (contains x !lst) with 
                                            [] ->
                                              lst := (x, f x) :: (!lst);
                                              f x        
                                          | (func, l) :: tl -> l 
;;

(*
  The following function makes a cached version for f that looks
  identical to f; users can't see that values are being cached 
*)

let make_cached_fun f = 
  let cf = new_cached_fun f in 
    function x -> apply_fun_with_cache cf x
;;


(*
let f x = x + 1;;
let cache_for_f = new_cached_fun f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 2;;
cache_for_f;;
apply_fun_with_cache cache_for_f 5;;
cache_for_f;;
let cf = make_cached_fun f;;
cf 4;;
cf 4;;


# val f : int -> int = <fun>
# val cache_for_f : ... 
# - : int = 2
# - : ...
# - : int = 2
# - : ...
# - : int = 3
# - : ...
# - : int = 6
# - : ...
# val cf : int -> int = <fun>
# - : int = 5
# - : int = 5
*)

