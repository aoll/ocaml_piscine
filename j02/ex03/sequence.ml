(*
let rec add_elem i n elem dst = match i with
	| i when i >= n -> dst
	| _ -> add_elem (i + 1) n (dst @ [elem])
in *)




(* 1
11
21
1211

1
[(1,1)]
11
[(2,1)]
21
[(1,2), (1,1)]
1211
[(1, 1);(1,2);(2,1)]
111221 *)

let sequence n =
	if n <= 0 then ""
	else
	begin

	let rec format l target = match l with
		| [] -> target
		| head::tail -> format tail (target ^ (string_of_int head))
	in

	let rec add src dst = match src with
		| [] -> dst
		| (x,y)::tail -> add tail (dst @ [x] @ [y])
	in

	let rec encode l =
		let rec loop l en x = match l with
		| [] -> en
		| head::next::tail when head = next -> loop  (next::tail) en (x + 1)
		|	head::tail -> loop tail (en @ [(x, head)]) 1
		in
		loop l [] 1
	in

	let rec s_loop i l_display = match i with
		| i when i = n -> format l_display ""
		| _ -> s_loop (i + 1) (add (encode l_display) [])
	in
	s_loop 1 [1]
	end

let () =

	print_endline ( sequence (-1)) ;
	print_endline ( sequence 0) ;
	print_endline ( sequence 1) ;
	print_endline ( sequence 2) ;
	print_endline ( sequence 3) ;
	print_endline ( sequence 4) ;
	print_endline ( sequence 5)
