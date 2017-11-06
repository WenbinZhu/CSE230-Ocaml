exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | NilExpr -> 
        "[]"

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: string * env -> value
 * 
 * Finds the most recent binding for a variable
 *)
let lookup (x,evn) = 
    match listAssoc (x,evn) with 
    | Some v -> v
    | None -> raise (MLFailure ("variable not bound: " ^ x))

(* eval : env * expr -> value
 * 
 * Evaluates an ML-nano expression e in the environment evn
 *)
let rec eval (evn,e) = 
    match e with 
    | NilExpr -> Nil
    | Const i -> Int i
    | True -> Bool true
    | False -> Bool false
    | Var s -> lookup (s, evn)
    | Let (s, e1, e2) -> eval ((s, eval (evn, e1)) :: evn, e2)
    | Letrec (s, e1, e2) -> 
        (match e1 with
        | Fun (_s, expr) -> eval ((s, Closure (evn, Some s, _s, expr)) :: evn, e2)
        | _ -> eval ((s, eval (evn, e1)) :: evn, e2))
    | Fun (s, e1) -> Closure (evn, None, s, e1)
    | App (f, e1) -> 
        (match f with 
        | Var "hd" -> 
            (match eval (evn, e1) with 
            | Pair (Int a, _) -> Int a
            | Pair (Bool a, _) -> Bool a
            | _ -> raise (MLFailure "Invalid expression"))
        | Var "tl" ->
            (match eval (evn, e1) with 
            | Pair (Int _, Nil) -> Nil
            | Pair (Bool _, Nil) -> Nil
            | Pair (Int _, Pair (Int a, b)) -> Pair (Int a, b)
            | Pair (Bool _, Pair (Bool a, b)) -> Pair (Bool a, b)
            | _ -> raise (MLFailure "Invalid expression"))
        | _ -> 
            (let c = eval (evn, f) in 
            match c with 
            | Closure (_evn, name, formal, expr) -> 
                (let v = eval (evn, e1) in 
                match name with 
                | None -> eval ((formal, v) :: _evn, expr)
                | Some s -> eval ((s, c) :: (formal, v) :: _evn, expr))
            | _ -> raise (MLFailure "Invalid expression")))
    | If (e1, e2, e3) -> 
        (let v1 = eval (evn, e1) in
        match v1 with 
        | Bool true -> eval (evn, e2)
        | Bool false -> eval (evn, e3)
        | _ -> raise (MLFailure "Invalid expression"))
    | Bin (e1, op, e2) -> 
        (let v1 = eval (evn, e1) in
        let v2 = eval (evn, e2) in
        match (v1, op, v2) with 
        | (Int a, Plus, Int b) -> Int (a + b) 
        | (Int a, Minus, Int b) -> Int (a - b)
        | (Int a, Mul, Int b) -> Int (a * b)
        | (Int a, Div, Int b) -> Int (a / b)
        | (Int a, Eq, Int b) -> Bool (a = b)
        | (Int a, Ne, Int b) -> Bool (a != b)
        | (Int a, Lt, Int b) -> Bool (a < b)
        | (Int a, Le, Int b) -> Bool (a <= b)
        | (Bool a, Eq, Bool b) -> Bool (a = b)
        | (Bool a, Ne, Bool b) -> Bool (a != b)
        | (Bool a, And, Bool b) -> Bool (a && b)
        | (Bool a, Or, Bool b) -> Bool (a || b)
        | (Int a, Cons, Nil) -> Pair (Int a, Nil)
        | (Int a, Cons, Pair (Int b, c)) -> Pair (Int a, Pair (Int b, c))
        | (Bool a, Cons, Nil) -> Pair (Bool a, Nil)
        | (Bool a, Cons, Pair (Bool b, c)) -> Pair (Bool a, Pair (Bool b, c))
        | _ -> raise (MLFailure "Invalid expression"))

(**********************     Testing Code  ******************************)
