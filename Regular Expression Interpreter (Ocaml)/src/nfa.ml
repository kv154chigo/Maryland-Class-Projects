open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
let merger lst lst2 =
	let f a run =
	if (List.mem run a) = false then run::a else a
	in List.fold_left f lst lst2
;; 

let contains num s lst =
	let f a lst =
	match lst with 
	|(x,y,z) -> if (num = x && s = y) then z::a else a
 in List.fold_left f [] lst
;;
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let f a lst = 
   a @ (contains lst s nfa.delta)
	in List.fold_left f [] qs
;;

let contains2 num lst =
 	let f a lst =
	match lst with 
	|(x,y,z) -> if (num = x && y = None) then z::a else a
 in List.fold_left f [] lst
;;

let rec helpreach nfa qs lst =
	let f a run =
	if List.mem run lst then
	merger a (run::(contains2 run nfa.delta)) 
	else
	merger a (helpreach nfa ((contains2 run nfa.delta) @ qs) (run::lst))
in List.fold_left f [] qs;;
;;
let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
 	(helpreach nfa qs [])
;;


let helper num s lst =
	let f a lst =
	match lst with 
	|(x,y,z) -> let n1 = match y with 
		|None -> a
		|Some n -> if ((Pervasives.compare x num) = 0 && s = n) then z::a else a in n1
 in List.fold_left f [] lst
;;

let rec contains3 nfa num lst =
	match lst with
	|[]->false
	|a::[] -> if List.length (move nfa num (Some a)) = 0 then false else (List.mem (List.nth (move nfa num (Some a)) 0) nfa.fs)
	|a::b -> let m1 = (move nfa num (Some a)) in
		match m1 with
		|[]->false
		|x::y -> (contains3 nfa m1 b)
	
	(*match lst with 
	|[]->false
	|x::[]-> (match (helper num x nfa.delta) with
		|[] -> false
		|a::b -> if (List.mem a nfa.fs) then true else false)
	|h::t -> (match (helper num h nfa.delta) with
		|[] -> false
		|a::b -> contains3 nfa a t)
;;
	*)
let rec help nfa lst =
	match lst with 
	|[]-> false
	|a::b -> if List.mem a nfa.fs then true else help nfa b
;;
let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let m1 =
	(e_closure nfa (let f a lst = (move nfa (e_closure (nfa) (a)) (Some lst)) in List.fold_left f ([nfa.q0]) (explode s))) in (help nfa m1);;
	

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)


let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
	let q qlist = 
	e_closure nfa qlist in 
	List.map q (let f lst =
	(move nfa qs (Some lst)) 
in List.map f nfa.sigma)
;;
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
	let q lst a =
	(qs, Some a, lst) in
	List.map2 q (new_states nfa qs) nfa.sigma
;;

let rec finalshelp nfa qs =
	match qs with
	|[] -> false
	|a::b -> if (List.mem a nfa.fs) = false then (finalshelp nfa b) else true
;;
let rec new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
	if (finalshelp nfa qs) = true then [qs] else []
;;

let rec final nfa lst = 
	match lst with 
	|[] -> []
	|h::t -> (new_finals nfa h) @ (final nfa t)
;;

let rightdelt lst =
	let f a delt =
	match delt with
	|(x,y,z)-> (match z with
		|[] -> a
		|h::t -> delt::a)
	in List.fold_left f [] lst
;;
let rec delta nfa lst =
	match lst with
	|[] -> []
	|h::t -> (rightdelt (new_trans nfa h)) @ (delta nfa t)
;;

let rec qs nfa lst =
	let f a lst2 =
	if (List.mem [lst2] a) = false then [lst2]::a else a in
	List.fold_left f lst nfa.qs
;;

let noext qs work newwork=
   let f a lst =
   if (List.mem lst qs) = false then lst::a else a
	in List.fold_left f newwork work
;;

let remove lst =
	let f a run =
	match run with
	|[]-> a
	|x::y -> run::a
in List.fold_left f [] lst;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
	 match work with
	 |[] -> dfa
	 |a::b-> let m1 = {sigma = dfa.sigma;
	  qs = remove (a::dfa.qs);
	  q0 = dfa.q0;
	  fs = merger dfa.fs (new_finals nfa a);
	  delta = merger dfa.delta (rightdelt (new_trans nfa a))} in (nfa_to_dfa_step nfa m1 (remove (noext m1.qs (new_states nfa a) b)))
;;
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
	let m1 = {sigma = nfa.sigma; qs = [(e_closure nfa [nfa.q0])]; q0 = (e_closure nfa [nfa.q0]); 
	fs =((new_finals nfa (e_closure nfa [nfa.q0])) @ (let f lst = [lst] in List.map f nfa.fs)); 
	delta = rightdelt (new_trans nfa (e_closure nfa [nfa.q0]))} in
	(nfa_to_dfa_step nfa m1 (remove (new_states nfa (e_closure nfa [nfa.q0]))));;


	(*let m1 = {sigma = nfa.sigma; qs = qs nfa (new_states nfa nfa.qs); q0 = [nfa.q0]; 
	fs = (final nfa (qs nfa (new_states nfa nfa.qs))); delta = (delta nfa (qs nfa (new_states nfa nfa.qs)))} in m1;;
*)

