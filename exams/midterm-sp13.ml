let length l = 
    let f acc ele = acc + 1 in
    List.fold_left f 0 l

let remove l x = 
    let f acc ele = 
        if ele = x then acc else acc @ [ele]
    in
    List.fold_left f [] l

let rec ith l i d = 
    match l with 
    | [] -> d
    | h :: t -> if i = 0 then h else ith t (i - 1) d

let rec update l i n = 
    match l with
    | [] -> []
    | h :: t -> if i = 0 then n :: t else h :: update t (i - 1) n

let rec update2 l i n d = 
    match l with 
    | [] -> if i = 0 then [n] else d :: update2 [] (i - 1) n d
    | h :: t -> if i = 0 then n :: t else h :: update2 t (i - 1) n d

let categorize f l = 
    let base = [] in
    let fold_fn acc elmt = 
        let idx = f elmt in
        update2 acc idx ((ith acc idx []) @ [elmt]) []
    in
    List.fold_left fold_fn base l