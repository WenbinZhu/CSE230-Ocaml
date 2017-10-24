(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 * 
 * Returns the sqaured sum of the input int list
 *)

let sqsum xs = 
  let f a x = a + x * x in
  let base = 0 in
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> ('a -> 'a)
 * 
 * Takes a list of functions [f1;...;fn] and return a piped function 
 * call on any x: fn(...(f2(f1 x))).
 *)

let pipe fs = 
  let f a x = fun v -> x (a v) in
  let base = fun v -> v in
    List.fold_left f base fs

(* sepConcat : string -> string list -> string
 * 
 * Takes a string separator and a string list, concatenates the strings
 * in the list by the separator.
 *)

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l
  
let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a list
 *
 * Clone x for n times as a list
 *)

let rec clone x n = 
  match n <= 0 with 
  | true  -> []
  | false -> x :: clone x (n - 1)

let rec padZero l1 l2 = 
  let d = List.length l1 - List.length l2 in
  (List.append (clone 0 (-d)) l1, List.append (clone 0 d) l2)

(* removeZero : int list -> int list
 *
 * Remove a prefix of trailing zeros in the list
 *)

let rec removeZero l = 
  match l with
  | [] -> []
  | h :: t -> if h = 0 then removeZero t else l

(* bigAdd : int list -> int list -> int list
 * 
 * Add two big integers represented as int lists
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
      let (c, r) = a in
      let (x1, x2) = x in
      let sum = x1 + x2 + c in
      ((sum / 10), (sum mod 10) :: r)
    in
    let base = (0, []) in
    let args = List.rev (List.combine (0 :: l1) (0 :: l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list
 * 
 * Multiply a digit with a big int
 *)

let rec mulByDigit i l = 
  let f a x = 
    let (c, r) = a in
    let mul = i * x + c in
    ((mul / 10), (mul mod 10) :: r)
  in
  let base = (0, []) in
  let (_, res) = List.fold_left f base (List.rev (0 :: l)) in
  
  removeZero res

(* bigMul : int list -> int list -> int list
 * 
 * Multiply two big integers represented as int lists
 *)

let bigMul l1 l2 = 
  let f a x = 
    let (z, r) = a in
    (0 :: z, bigAdd r (List.append (mulByDigit x l1) z))
  in
  let base = ([], []) in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res
