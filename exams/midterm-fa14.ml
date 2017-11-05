type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;

let rec rename_var e n1 n2 = 
	match e with
	| Var s -> if s = n1 then Var n2 else e
	| Op (s, e1, e2) -> Op (s, rename_var e1 n1 n2, rename_var e2 n1 n2)
	| _ -> e

let to_str e = 
	let rec str_helper e top_level =
		match e with 
		| Const c -> string_of_int c
		| Var s -> s
		| Op (s, e1, e2) -> let r = (str_helper e1 false) ^ s ^ (str_helper e2 false) in
							if top_level then r else "(" ^ r ^ ")"

		in
		str_helper e true;;

let average_if f l = 
    let folding_fn (acc, num) ele = 
        if f ele then (acc + ele, num + 1) else (acc, num)
    in
    let base = (0, 0) in
    let (sum, total) = List.fold_left folding_fn base l in
    if total = 0 then 0 else sum / total

let length_2 l = 
    List.fold_left (+) 0 (List.map (fun ll -> List.length ll) l)

let length_3 l = 
    List.fold_left (+) 0 (List.map (fun ll -> length_2 ll) l)