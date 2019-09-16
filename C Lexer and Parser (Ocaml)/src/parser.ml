open SmallCTypes
open Utils
open TokenTypes

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

    (* start your code here *)
let lookahead toks = match toks with
   h::t -> h
  | _ ->  raise (InvalidInputException "did not reach EOF")

let rec parse_expr toks =  let(t, exp) = parse_Or toks in
  (t,exp)
and parse_Or (toks : token list) : (token list * expr) =
  let (t,m) = parse_And toks in
  match lookahead t with
  |Tok_Or -> let t' = match_token t Tok_Or in 
                let (t'',s) = parse_Or t' in
                (t'', Or (m, s))
  |_ -> t, m
and parse_And (toks : token list) : (token list * expr) =
  let (t,l) = parse_NE toks in
  match lookahead t with
  |Tok_And -> let t' = match_token t Tok_And in 
                let (t'',m) = parse_And t' in
                (t'', And (l, m))
  |_ -> t, l
(* Parses the L rule. *)
and parse_NE (toks : token list) : (token list * expr) =
  let (t,k) = parse_E toks in
  match lookahead t with
  |Tok_NotEqual -> let t' = match_token t Tok_NotEqual in 
                let (t'',l) = parse_NE t' in
                (t'', NotEqual (k, l))
  |_ -> t, k
(* Parses the L rule. *)
and parse_E (toks : token list) : (token list * expr) =
  let (t,j) = parse_G toks in
  match lookahead t with
  |Tok_Equal -> let t' = match_token t Tok_Equal in 
                let (t'',k) = parse_E t' in
                (t'', Equal (j, k))
  |_ -> t, j
(* Parses the L rule. *)
and parse_G (toks : token list) : (token list * expr) =
  let (t,f) = parse_L toks in
  match lookahead t with
  |Tok_Greater -> let t' = match_token t Tok_Greater in 
                let (t'',j) = parse_G t' in
                (t'', Greater (f, j))
  |_ -> t, f
(* Parses the L rule. *)
and parse_L (toks : token list) : (token list * expr) =
  let (t,g) = parse_GE toks in
  match lookahead t with
  |Tok_Less -> let t' = match_token t Tok_Less in 
                let (t'',f) = parse_L t' in
                (t'', Less (g, f))
  |_ -> t, g
(* Parses the N rule. *)
and parse_GE (toks : token list) : (token list * expr) =
  let (t,h) = parse_LE toks in
  match lookahead t with
  |Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in 
                let (t'',g) = parse_GE t' in
                (t'', GreaterEqual (h, g))
  |_ -> t, h
(* Parses the N rule. *)
and parse_LE (toks : token list) : (token list * expr) =
  let (t,i) = parse_S toks in
  match lookahead t with
  |Tok_LessEqual -> let t' = match_token t Tok_LessEqual in 
                let (t'',h) = parse_LE t' in
                (t'', LessEqual (i, h))
  |_ -> t, i
(* Parses the N rule. *)
and parse_S (toks : token list) : (token list * expr) =
  let (t,a) = parse_A toks in
  match lookahead t with
  |Tok_Sub -> let t' = match_token t Tok_Sub in 
                let (t'',i) = parse_S t' in
                (t'', Sub (a, i))
  |_ -> t, a
(* Parses the A rule. *)
and parse_A (toks : token list) : (token list * expr) =
  let (t,b) = parse_M toks in
  match lookahead t with
  |Tok_Add -> let t' = match_token t Tok_Add in 
                let (t'',a) = parse_A t' in
                (t'', Add (b, a))
  |_ -> t, b
(* Parses the M rule. *)
and parse_M (toks : token list) : (token list * expr) =
  let (t,c) = parse_D toks in
  match lookahead t with
  |Tok_Mult -> let t' = match_token t Tok_Mult in 
                let (t'',b) = parse_M t' in
                (t'', Mult (c, b))
  |_ -> t, c
(* Parses the D rule. *)
and parse_D (toks : token list) : (token list * expr) =
  let (t,d) = parse_P toks in
  match lookahead t with
  |Tok_Div -> let t' = match_token t Tok_Div in 
                let (t'',c) = parse_D t' in
                (t'', Div (d, c))
  |_ -> t, d
(* Parses the P rule. *)
and parse_P (toks : token list) : (token list * expr) =
  let (t,e) = parse_N toks in
  match lookahead t with
  |Tok_Pow -> let t' = match_token t Tok_Pow in 
                let (t'',d) = parse_P t' in
                (t'', Pow (e, d))
  |_ -> t, e
(*and parse_Not (toks : token list) : (token list * expr) =
  let (t,nott) = parse_N toks in
  match lookahead t with
  |Tok_Not -> let t' = match_token t Tok_Not in 
                let (t'',e) = parse_Not t' in
                (t'', Not (e))
  |_ -> t, nott*)
and parse_N (toks : token list) : (token list * expr) =
  match lookahead toks with 
  | Tok_Int i -> let t = match_token toks (Tok_Int i) in
                  (t, Int i)
  | Tok_Bool i-> let t = match_token toks (Tok_Bool i) in
                  (t, Bool i)
  | Tok_ID i -> let t = match_token toks (Tok_ID i) in
                  (t, ID i)
  | Tok_LParen -> let t = match_token toks Tok_LParen in 
                  let (t', s) = parse_expr t in
                  let t'' = match_token t' Tok_RParen in
                  (t'', s)
  |Tok_Not -> let t = match_token toks Tok_Not in 
                let (t',e) = parse_N t in
                (t', Not (e))
  |_ ->  raise (InvalidInputException "did not reach EOF")

let rec parse_stmt (toks : token list) : (token list * stmt) =
  match lookahead toks with
  | Tok_Int_Type -> let t = match_token toks Tok_Int_Type in 
                let (t1,s) = parse_expr t in
                (match s with
                | ID i -> let t2 = match_token t1 Tok_Semi in 
                        let (t3,a) = parse_stmt t2 in
                        (t3, Seq(Declare (Int_Type, i),a))
                | _ ->  raise (InvalidInputException "did not reach EOF"))
  | Tok_Bool_Type -> let t = match_token toks Tok_Bool_Type in 
                let (t1,s) = parse_expr t in
                (match s with
                | ID i -> let t2 = match_token t1 Tok_Semi in 
                        let (t3,a) = parse_stmt t2 in
                        (t3, Seq(Declare (Bool_Type, i),a))
                | _ ->  raise (InvalidInputException "did not reach EOF"))
  | (Tok_ID i) -> let t = match_token toks (Tok_ID i) in 
                let t1 = match_token t Tok_Assign in
                let (t2,s) = parse_expr t1 in
                let t3 = match_token t2 Tok_Semi in
                let (t4,a) = parse_stmt t3 in 
                (t4, Seq(Assign(i,s),a))
  | Tok_Print -> let t = match_token toks Tok_Print in
                let (t1,s) = parse_expr t in 
                let t2 = match_token t1 Tok_Semi in 
                let (t3, a) = parse_stmt t2 in
                (t3, Seq(Print(s),a))
  | Tok_If -> let t = match_token toks Tok_If in 
              let (t1,s) = parse_expr t in 
              let t2 = match_token t1 Tok_LBrace in 
              let (t3,a) = parse_stmt t2 in 
              let t4 = match_token t3 Tok_RBrace in 
               (match lookahead t4 with 
               |Tok_Else -> let t5 = match_token t4 Tok_Else in 
                let t6 = match_token t5 Tok_LBrace in 
                let (t7,b) = parse_stmt t6 in 
                let t8 = match_token t7 Tok_RBrace in 
                let (t9,c) = parse_stmt t8 in 
                (t9, Seq(If((s),(a),(b)),c))
               |_-> let (t5,b) = parse_stmt t4 in  
                   (t5, Seq(If((s),(a),(NoOp)),b)))
  | Tok_Do -> let t = match_token toks Tok_Do in 
              let t1 = match_token t Tok_LBrace in 
              let (t2,s) = parse_stmt t1 in 
              let t3 = match_token t2 Tok_RBrace in 
              let t4 = match_token t3 Tok_While in 
              let (t5,a) = parse_expr t4 in 
              let t6 = match_token t5 Tok_Semi in 
              let (t7,b) = parse_stmt t6 in 
              (t7, Seq(DoWhile(s,a),b))
  | Tok_While -> let t = match_token toks Tok_While in 
                 let (t1, s) = parse_expr t in 
                 let t2 = match_token t1 Tok_LBrace in 
                 let (t3,a) = parse_stmt t2 in 
                 let t4 = match_token t3 Tok_RBrace in 
                 let (t5,b) = parse_stmt t4 in 
                 (t5, Seq(While(s,a),b))
  |_ -> (toks,NoOp)


let rec parse_main toks = 
  let(t, stmt) = parse_Main toks in
   if t<> [EOF] then
    raise (InvalidInputException "did not reach EOF")
  else 
  stmt
and parse_Main (toks : token list) : (token list * stmt) =
  match lookahead toks with 
  | Tok_Int_Type -> let t = match_token toks Tok_Int_Type in 
                  let t1 = match_token t Tok_Main in
                  let t2 =  match_token t1 Tok_LParen in
                  let t3 =  match_token t2 Tok_RParen in
                  let t4 =  match_token t3 Tok_LBrace in
                  let (t5, s) = parse_stmt t4 in
                  match lookahead t5 with
                  |Tok_RBrace -> let t6 = match_token t5 Tok_RBrace in (t6,s)
                  |EOF -> (t5,s)
  |_ -> raise (InvalidInputException "did not reach EOF")

