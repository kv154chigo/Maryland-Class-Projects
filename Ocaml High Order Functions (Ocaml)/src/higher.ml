open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let len lst =
	let f length lst = 
	1 + length 
	in
	fold f 0 lst
;;

let count_greater lst target = 
	let f num element = 
	if element > target then 1 + num
	else	num
in 	
	fold f 0 lst
;;

let greater_tuple lst = 
	let f element =
	(element,(count_greater lst element)) in
	map f lst 
;;

let flat_pair lst = 
	let func full (element: 'a * 'a) =
	let (x, y) = element in
	append full [x;y] in 
	fold func [] lst
	
;;

let helper lst target =
	let f full element =
	if element <= target then
	element::full 
else full in
	fold f [] lst
	;;

let rm lst target = 
	rev (helper lst target)
;;
