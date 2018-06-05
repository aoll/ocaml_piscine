module Color =
struct
type t = Spade | Heart | Diamond | Club

let all = [Spade ; Heart ; Diamond ; Club]

let toString c = match c with
	| Spade -> "S"
	| Heart -> "H"
	| Diamond -> "D"
	| Club -> "C"

let toStringVerbose c = match c with
	| Spade -> "Spade"
	| Heart -> "Heart"
	| Diamond -> "Diamond"
	| Club -> "Club"

end

module Value =
struct
type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
(** The list of all values of type t *)
let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]
(** Interger representation of a card value, from 1 for T2 to 13 for As *)
let toInt t = match t with
	| T2 -> 1
	| T3 -> 2
	| T4 -> 3
	| T5 -> 4
	| T6 -> 5
	| T7 -> 6
	| T8 -> 7
	| T9 -> 8
	| T10 -> 9
	| Jack -> 10
	| Queen -> 11
	| King -> 12
	| As	-> 13

(** returns "2", ..., "10", "J", "Q", "K" or "A" *)
let toString t = match t with
	| T2 -> "2"
	| T3 -> "3"
	| T4 -> "4"
	| T5 -> "5"
	| T6 -> "6"
	| T7 -> "7"
	| T8 -> "8"
	| T9 -> "9"
	| T10 -> "10"
	| Jack -> "J"
	| Queen -> "Q"
	| King -> "K"
	| As	-> "A"

(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
let toStringVerbose t = match t with
	| T2 -> "2"
	| T3 -> "3"
	| T4 -> "4"
	| T5 -> "5"
	| T6 -> "6"
	| T7 -> "7"
	| T8 -> "8"
	| T9 -> "9"
	| T10 -> "10"
	| Jack -> "Jack"
	| Queen -> "Queen"
	| King -> "King"
	| As	-> "As"


(** Returns the next value, or calls invalid_arg if argument is As *)
let next t = match t with
	| T2 -> T3
	| T3 -> T4
	| T4 -> T5
	| T5 -> T6
	| T6 -> T7
	| T7 -> T8
	| T8 -> T9
	| T9 -> T10
	| T10 -> Jack
	| Jack -> Queen
	| Queen -> King
	| King -> As
	| As	-> invalid_arg "As is the best"


(** Returns the previous value, or calls invalid_arg if argument is T2 *)
let previous t = match t with
	| T2 -> invalid_arg "2 is the best"
	| T3 -> T2
	| T4 -> T3
	| T5 -> T4
	| T6 -> T5
	| T7 -> T6
	| T8 -> T7
	| T9 -> T8
	| T10 -> T9
	| Jack -> T10
	| Queen -> Jack
	| King -> Queen
	| As	-> King

end

type t

let newCard value color =
	(value,color)

let allSpades =
let rec loopValue l1 c dst = match l1 with
	| h::t -> loopValue t c (dst @ [newCard h c])
	| [] -> dst
in
loopValue Value.all Color.Spade



let allHearts =
let rec loopValue l1 c dst = match l1 with
	| h::t -> loopValue t c (dst @ [newCard h c])
	| [] -> dst
in
loopValue Value.all Color.Heart

let allDiamonds =
let rec loopValue l1 c dst = match l1 with
	| h::t -> loopValue t c (dst @ [newCard h c])
	| [] -> dst
in
loopValue Value.all Color.Diamond

let allClubs =
let rec loopValue l1 c dst = match l1 with
	| h::t -> loopValue t c (dst @ [newCard h c])
	| [] -> dst
in
loopValue Value.all Color.Club

let all =
	let rec loopColor v l2 dst = match l2 with
	| h::t -> loopColor v t (dst @ [newCard v h])
	| [] -> dst
	in
	let rec loopValue l1 l2 dst = match l1 with
		| h::t -> loopValue t l2 (loopColor h l2 dst)
		| [] -> dst
	in
	loopValue Value.all Color.all


let getValue t = match t with
| (v,_) -> v
(* | _ -> *)

let getColor t = match t with
	| (_,c) -> c
	(* | _ -> _ *)

let toString t = match t with
	| (v,c) ->    (Value.toString v) ^ (Color.toString  c)
	| _ -> ""


let toStringVerbose t = match t with
	| (v,c) ->  "Card(" ^(Value.toStringVerbose v) ^ "," ^(Color.toStringVerbose  c) ^ ")"
	| _ -> ""


let compare  t1  t2 =
	let getValue t = match t with
		| (v,c) ->  v
		| (_,_) -> (-1)
	in
	let v1 = getValue t1 in
	let v2 = getValue t2 in
	if v1 = v2 then 0
	else if v1 < v2 then -1
	else 1


let max t1 t2 =
	if (compare t1 t2) >= 0 then t1
	else t2

let min t1 t2 =
	if (compare t1 t2) <= 0 then t1
	else t2

let best l = match l with
| h::t -> List.fold_left max h t
| [] -> invalid_arg "Empty list"

	(* List.fold_left max (List.hd l) l *)

let isOf t c = match t with
| (_,c1) when c1 = c ->  true
| _ -> false

let isSpade t =  match t with
	| (_,Color.Spade) -> true
	| _ -> false

let isHeart t =  match t with
	| (_,Color.Heart) -> true
	| _ -> false

let isDiamond t =  match t with
	| (_,Color.Diamond) -> true
	| _ -> false

let isClub t =  match t with
	| (_,Color.Club) -> true
	| _ -> false
