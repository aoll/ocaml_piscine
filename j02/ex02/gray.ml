(* 0 1

0 1 -> 1 0
00 01 -> 11 10
01 00 -> 11 10
00 01 11 10

00 01 11 10 -> 10 11 01 00

000 001 011 010 110 111 101 100 *)

let gray n =
	if n = 0 then print_char '\n'
	else
	begin

		let rec p l = match l with
		| [] -> print_char '\n'
		| head::body::tail -> print_string head ; print_char ' ' ; p (body::tail)
		| head::tail -> print_string head ; print_char '\n'
		in

		let rec reverse l_to_re l_dst = match l_to_re with
		| [] -> l_dst
		| head::tail -> reverse tail (head::l_dst)
		in

		let rec add_prefix l_to pre  = match l_to with
		| [] -> []
		| head::tail -> ((pre ^ head)::(add_prefix tail pre))
		in

		let rec loop i l1  = match i with
		| i when i >= n -> p l1
		| _ -> loop (i + 1) (reverse (reverse (add_prefix  l1 "0") []) (add_prefix  (reverse l1 []) "1"))
		in

		loop 1 ["0"; "1"]
	end

let () =
	print_string "gray 0: " ; gray 0 ;
	print_string "gray 1: " ; gray 1 ;
	print_string "gray 2: " ; gray 2 ;
	print_string "gray 3: " ; gray 3
