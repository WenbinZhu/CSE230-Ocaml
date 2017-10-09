(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int
 *
 * Apply pattern matching to calculate sum:
 * Pattern 1: empty list, return 0.
 * Pattern 2: non-empty list, get head of list and
 *  		  recursively calculate the tail list.
 *)

let rec sumList l =
    match l with
    | [] -> 0
    | h :: t -> h + (sumList t);;


(* digitsOfInt : int -> int list
 *
 * Create an inner function _scale (int -> int) to get
 * the scale of the input int with pattern matching:
 * recursively divide the int by 10 and times the result
 * by 10 util it becomes zero, which returns 1.

 * Then use pattern mathing again to get the digits of
 * int list from the scale: recursively get the first
 * digit of the int by dividing it by scale, use cons
 * operator to return the list, with first digit as the
 * head and the recursive result of the last digits as
 * the tail. Stop the recursion if the input int has
 * only one digit.
 *)

let rec digitsOfInt n =
    let rec _scale m =
        match (m / 10) > 0 with
        | true -> 10 * _scale (m / 10)
        | false -> 1 in

    let p = _scale n in

    match n > 9 with
    | true -> (n / p) :: digitsOfInt(n mod p)
    | false -> [n];;


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)

let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence : int -> int
 *
 * First get sum of digits using the above functions: get a list
 * of digits and then sum the list. Then apply pattern matching
 * to get the additive persistence: recursively call itself with
 * sum as input util the input int has only one digit and return 1.
 * Within each recursive call, add one to the result.
 *)

let rec additivePersistence n =
    let sum = sumList (digits n) in

    match sum < 10 with
    | true -> 1
    | false -> additivePersistence(sum) + 1;;


(* digitalRoot : int -> int
 *
 * First get sum of digits using the above functions: get a list
 * of digits and then sum the list. Then apply pattern matching
 * to get the digital root: recursively call itself with sum as
 * input util the input int has only one digit and return it.
 *)

let rec digitalRoot n =
    let sum = sumList (digits n) in

    match sum < 10 with
    | true -> sum
    | false -> digitalRoot(sum);;


(* listReverse : 'a list -> 'a list
 *
 * Create an inner function _reverse ('a list -> 'a list -> 'a list)
 * which has two lists as parameters. Recursively get the head of
 * the second list and use cons operator to concat this head and the
 * first list to form a new list and pass this new list to the
 * recursive call as the first parameter, and the tail as the
 * second parameter until the second list is empty. At this time,
 * return the first list since the first list contains all the elements
 * in the second list in reverse order.
 *
 * Call this inner function with an empty list and the input list
 * to get the reversed list.
 *)

let rec listReverse l =
    let rec _reverse l1 l2 =
        match l2 with
        | [] -> l1
        | h :: t -> _reverse (h :: l1) t in

    _reverse [] l;;

(* explode : string -> char list
 * (explode s) is the list of characters in the string s in the order in
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s =
  let rec _exp i =
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(* palindrome : string -> bool
 *
 * Use explode to get the char list representation of the input string,
 * call listReverse to get the reversed char list and use = operator to
 * check if the char list is identical to the reversed list.
 *)

let palindrome w =
    let exp = explode w in

    exp = listReverse(exp)

(************** Add Testing Code Here ***************)
