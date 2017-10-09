(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc : int * string * (string * int) list -> int
 * 
 * Apply pattern match on l. Match the head and tail of l. If 
 * the first element of head pair equals k, then return the 
 * second element of the head pair, else recursively call assoc 
 * on the tail list, util l is empty, in which case, return the
 * default value d.
 *)

let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | h :: t -> let (k', v') = h in
              if k' = k then v' else assoc (d, k, t)


(* removeDuplicates : 'a list -> 'a list
 *
 * Create a helper function : 'a list * 'a list -> 'a list which
 * takes a pair of two list: seen list and rest list. Match the
 * rest list, if the head of rest list is already in seen list,
 * which means the head is a duplicate. Otherwise use cons operator
 * to add this head to the seen list. Recursively call the helper
 * function itself until rest list becomes empty and return seen list.
 * 
 * Call the helper funcion in removeDuplicates with an empty list as 
 * seen list and the original list l as rest list. Reverse the result
 * of the helper function call to keep the original order in l.
 *)

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h :: seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile : ('a -> 'a * bool) * 'a -> 'a
 *
 * Call f on b and return a pair (b', c'): 'a * bool. If c' is true,
 * continue call f on b' until c' is false and return b'
 *)

(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
  let (b', c') = f b in
  if c' then wwhile (f, b') else b'


(* fixpoint : ('a -> 'a) * 'a -> 'a
 * 
 * Call wwhile to get the fixpoint of function f on b.
 * The first argument passed to wwhile is a function g that takes
 * x: 'a and returns a pair: 'a * bool, which stands for the result
 * of f(x) and whether x = f(x) respectively. The second argument
 * passed to wwhile is just b.
 *)

(* fill in the code wherever it says : failwith "to be written" *)
let fixpoint (f,b) = wwhile ((let rec g x = let x' = f x in (x', x <> x') in g),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) =  
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
