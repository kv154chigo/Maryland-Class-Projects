open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int_tree * int_tree * int

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(IntLeaf, IntLeaf, x)
  | IntNode (l, r, y) when x > y -> IntNode (l, int_insert x r, y)
  | IntNode (l, r, y) when x = y -> t
  | IntNode (l, r, y) -> IntNode (int_insert x l, r, y)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (l, r, y) when x > y -> int_mem x r
  | IntNode (l, r, y) when x = y -> true
  | IntNode (l, r, y) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with 
  |IntLeaf -> 0
  |IntNode (l, r, y) -> 1 + int_size l + int_size r 

let rec int_max t = 
  match t with
  |IntLeaf -> raise (Invalid_argument("int_max"))
  |IntNode (l, r, y) when int_size r = 0 -> y
  |IntNode (l, r, y) -> int_max r



let rec int_insert_all lst t = 
  let f tree element =
    int_insert element tree in
    fold f t lst
  ;;

let rec int_as_list t = 
  match t with
  |IntLeaf -> []
  |IntNode (l, r, y) -> append (int_as_list l) ((y)::(int_as_list r))

  

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec help_insert f x t =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when f x y > 0 -> Node (y, l, help_insert f x r)
  | Node (y, l, r) when f x y = 0-> t
  | Node (y, l, r) -> Node (y, help_insert f x l, r)

let rec pinsert x t = 
  match t with 
  | (f,Leaf) -> (f, Node(x,Leaf,Leaf))
  | (f, Node(y, l, r)) when f x y > 0 -> (f, Node(y, l, help_insert f x r))
  | (f, Node(y, l, r)) when f x y = 0 -> t
  | (f, Node(y, l, r)) -> (f, Node(y, help_insert f x l, r))

let rec pmem x t =  
  match t with
  | (f,Leaf) -> false
  | (f, Node(y, l, r)) when f x y > 0 -> pmem x (f,r)
  | (f, Node(y, l, r)) when f x y = 0 -> true
  | (f, Node(y, l, r)) -> pmem x (f,l)


let pinsert_all lst t =  
  let f tree element =
    pinsert element tree in
    fold f t lst
  ;;

let rec p_as_list t =
 match t with
  |(f,Leaf) -> []
  |(f, Node(y, l, r)) -> append (p_as_list (f,l)) ((y)::(p_as_list (f,r)))


let pmap f t = 
   match t with
  |(func,Leaf) -> t
  |(func, Node(y, l, r)) -> pinsert_all (map f (p_as_list t)) (empty_ptree func)

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = 
  if int_size g.nodes = 0 then true else false
;;

let graph_size g = 
  int_size g.nodes
;;

let is_dst n e = 
  if n = e.dst then true else false
;;

let src_edges n g = 
  let f full elements =
  if n = elements.src then
  elements::full else 
  full in
  fold f [] g.edges
;;

let contains lst n =
  let f num element = 
  if element = n then 1 + num
  else  num
in  
  fold f 0 lst
;;

let rec help_reach n g lst =
  let f full elements =
  if n = elements.src && n = elements.dst then
  append full (n::lst)
  else if (contains lst n) > 1 then 
  append full [elements.src;elements.dst]
  else if n = elements.src then
  append full (help_reach elements.dst g (n::lst))
  else if n = elements.dst then
  append full (n::lst)
  else full in
  fold f [] g.edges 
;; 

let reachable n g = 
  int_insert_all (help_reach n g []) (empty_int_tree)
;;