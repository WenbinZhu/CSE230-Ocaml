let count l x = 
	let f count ele = if ele = x then count + 1 else count in
	List.fold_left f 0 l

let make_palyndrome l = 
	let f ll ele = ele :: ll in
	List.fold_left f l l

let fold_2 f b l = 
    let fn (acc, idx) ele = 
        ((f acc ele idx), idx + 1)
    in
    let (res, _) = List.fold_left fn (b, 0) l in
    res

let rec ith l i d = 
    let f acc ele idx = 
        if idx = i then ele else acc
    in
    fold_2 f d l

type 'a fun_tree =
    | Leaf of ('a -> 'a)
    | Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x = 
    match t with 
    | Leaf f -> f x
    | Node (l, r) -> apply_all r (apply_all l x)

let rec compose t1 t2 = 
    match (t1, t2) with 
    | (Leaf f1, Leaf f2) -> Leaf (fun x -> f1 (f2 x))
    | (Node (l1, r1), Node (l2, r2)) -> Node (compose l1 r1, compose l2 r2)
    | _ -> compose t1 t2