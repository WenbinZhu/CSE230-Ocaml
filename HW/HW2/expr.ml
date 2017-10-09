(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	


(* exprToString : expr -> string
 *
 * Apply pattern matching to match each part of expr and 
 * return the corresponding string representation.
 *)

let rec exprToString e = 
  match e with 
  | VarX -> "x"
  | VarY -> "y"
  | Sine exp -> "sin(pi*" ^ (exprToString exp) ^ ")"
  | Cosine exp -> "cos(pi*" ^ (exprToString exp) ^ ")"
  | Average (exp1, exp2) -> "((" ^ (exprToString exp1) ^ "+" ^ (exprToString exp2) ^ ")/2)"
  | Times (exp1, exp2) -> (exprToString exp1) ^ "*" ^ (exprToString exp2)
  | Thresh (exp1, exp2, exp3, exp4) -> "(" ^ (exprToString exp1) ^ "<" ^ (exprToString exp2) ^ 
                                       "?" ^ (exprToString exp3) ^ ":" ^ (exprToString exp4) ^ ")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)


let pi = 4.0 *. atan 1.0


(* eval : expr * float * float -> float
 * 
 * Apply pattern matching to each type of e. If e is VarX or VarY,
 * just return x or y. For other types, recursively eval on the operands.
 *)

let rec eval (e,x,y) = 
  match e with
  | VarX -> x
  | VarY -> y
  | Sine exp -> sin (pi *. eval (exp, x, y))
  | Cosine exp -> cos (pi *. eval (exp, x, y))
  | Average (exp1, exp2) -> (eval(exp1, x, y) +. eval(exp2, x, y)) /. 2.0
  | Times (exp1, exp2) -> eval(exp1, x, y) *. eval(exp2, x, y)
  | Thresh (exp1, exp2, exp3, exp4) -> if eval(exp1, x, y) < eval(exp2, x, y) then eval(exp3, x, y) else eval(exp4, x, y)


(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
