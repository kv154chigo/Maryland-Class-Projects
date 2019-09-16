open SmallCTypes
open EvalUtils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec pow a b =
	match b with
	|0 -> 1
	|1 -> a 
	|x-> a * (pow a (b-1))

let rec eval_expr env e = match e with
	|Int x -> Int_Val x
	|Bool x -> Bool_Val x
	|ID x -> if (List.mem_assoc x env) then (List.assoc x env) else raise (DeclareError "Dec Error")
    |Add (x,y) -> let n1 = eval_expr env x in
			  let n2 = eval_expr env y in
			  let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> Int_Val (a+b)
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
	|Sub (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 =  match n1,n2 with
			  |Int_Val a, Int_Val b -> Int_Val (a-b)
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |Mult (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> Int_Val (a*b)
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |Div (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 =  match n1,n2 with
			  |Int_Val a, Int_Val 0 -> raise (DivByZeroError)
			  |Int_Val a, Int_Val b -> Int_Val (a/b)
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |Pow (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 =match n1,n2 with
			  |Int_Val a, Int_Val b -> Int_Val (pow a b)
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
	|Not x -> let n1 = eval_expr env x in 
	let n2 = match n1 with
		|Bool_Val true -> Bool_Val false
		|Bool_Val false -> Bool_Val true
		|_ -> raise (TypeError "Incorrect Type")
	in n2
	|Greater (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> if a > b then Bool_Val true else Bool_Val false
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |Less (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> if a < b then Bool_Val true else Bool_Val false
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |GreaterEqual (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> if a >= b then Bool_Val true else Bool_Val false
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
 	|LessEqual (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in
	let n3 = match n1,n2 with
			  |Int_Val a, Int_Val b -> if a <= b then Bool_Val true else Bool_Val false
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
	|Equal (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in 
	let n3 = match n1,n2 with
		|Bool_Val a, Bool_Val b -> if a = b then Bool_Val true else Bool_Val false
		|Int_Val a, Int_Val b -> if a = b then Bool_Val true else Bool_Val false
		|_,_ -> raise (TypeError "Incorrect Type")
		in n3
	|NotEqual (x,y) -> let n1 = eval_expr env x in
	let n2 = eval_expr env y in 
	let n3 = match n1,n2 with
		|Bool_Val a, Bool_Val b -> if a != b then Bool_Val true else Bool_Val false
		|Int_Val a, Int_Val b -> if a != b then Bool_Val true else Bool_Val false
		|_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |Or (x,y) -> let n1 = eval_expr env x in 
    let n2 = eval_expr env y in
    let n3 =  match n1,n2 with 
    		  |Bool_Val true, Bool_Val true -> Bool_Val true
    		  |Bool_Val true, Bool_Val false -> Bool_Val true
    		  |Bool_Val false, Bool_Val true -> Bool_Val true
    		  |Bool_Val false, Bool_Val false -> Bool_Val false
			  |_,_ -> raise (TypeError "Incorrect Type")
			  in n3
    |And (x,y) -> let n1 = eval_expr env x in 
    let n2 = eval_expr env y in
    let n3 = match n1,n2 with
    		 |Bool_Val true, Bool_Val true -> Bool_Val true
    		 |Bool_Val true, Bool_Val false -> Bool_Val false
    		 |Bool_Val false, Bool_Val true -> Bool_Val false
    	     |Bool_Val false, Bool_Val false -> Bool_Val false
		     |_,_ -> raise (TypeError "Incorrect Type")
		 in n3
;;
let rec eval_stmt env s = match s with
	|NoOp -> env
	|Seq (x,y) -> let n1 = eval_stmt env x in
				  let n2 = eval_stmt n1 y in n2
	|Declare (x,y) -> let n1 = if (List.mem_assoc y env) then raise (DeclareError "Dec Error")
		else match x with
		|Int_Type -> ((y,Int_Val 0))::(env)
		|Bool_Type -> ((y, Bool_Val false))::(env)
	in n1;
	|Assign (x,y) -> let n1 = if (List.mem_assoc x env) = false then raise (DeclareError "Dec Error")
	else eval_expr env y in
	let n2 = match n1, (List.assoc x env) with
	|Int_Val a, Int_Val b -> (x, n1)::(List.remove_assoc x env)
	|Bool_Val a, Bool_Val b -> (x, n1)::(List.remove_assoc x env)
	|_,_ ->  raise (TypeError "Incorrect Type") 
	in n2 
	|If (x,y,z) -> let n1 = eval_expr env x in 
	let n2 = match n1 with
		|Bool_Val true -> eval_stmt env y
		|Bool_Val false -> eval_stmt env z 
		| _ -> raise (TypeError "Incorrect Type")  
	in n2
	|While (x,y) -> let n1 = eval_expr env x in
	let n2 = match n1 with
		|Bool_Val true -> let n3 = eval_stmt env y in let n4 = eval_stmt n3 s in n4
		|Bool_Val false -> env
		| _ -> raise (TypeError "Incorrect Type")  
	in n2
	|DoWhile (x,y) -> let n1 = eval_expr env y in
	let n2 = match n1 with
		|Bool_Val true -> let n3 = eval_stmt env x in let n4 = eval_stmt n3 s in n4
		|Bool_Val false -> env
		| _ -> raise (TypeError "Incorrect Type")  
	in n2
	|Print x -> let n1 = eval_expr env x in 
		let n2 = match n1 with
		|Int_Val a -> let n2 = print_output_int a in let n3 = print_output_string "\n" in env
		|Bool_Val a -> let n2 = print_output_bool a in let n3 = print_output_string "\n" in env
	in n2
		
