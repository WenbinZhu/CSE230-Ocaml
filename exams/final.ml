(* fa13 *)

let rec insert l i = 
  match l with
  | [] -> [i]
  | h :: t -> if i <= h then i :: h :: t else h :: (insert t i)

let insert_sort = List.fold_left insert []

type expr =
  | Var of string
  | Const of int
  | Plus of expr * expr

let rec simpl e = 
  match e with 
  | Plus (e1, e2) ->
      (let e1' = simpl e1 in
      let e2' = simpl e2 in
      match (e1', e2') with
      | (Const a, Const b) -> Const (a + b)
      | _ -> Plus (e1', e2'))
  | _ -> e


(* sp13 *)

let count f l = 
  let func acc ele = if f ele then acc + 1 else acc in
  List.fold_left func 0 l

let stretch l = 
  let func acc ele = acc @ [ele; ele] in
  List.fold_left func [] l

type 'a tree =
  | Empty
  | Node of 'a * 'a tree list

exception Mismatch

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
  | _ -> raise Mismatch

let rec tree_zip t1 t2 = 
  match (t1, t2) with 
  | (Empty, Empty) -> Empty
  | (Node(n1, l1), Node(n2, l2)) ->
      let l = zip l1 l2 in
      let lm = List.map (fun (m1, m2) -> tree_zip m1 m2) l in
      Node((n1, n2), lm)
  | _ -> raise Mismatch


(* wi13 *)

let sum_matrix ll = 
  let fn acc ele = 
    acc + List.fold_left (+) 0 ele in
  List.fold_left fn 0 ll
