
	let rec encode l =
		let rec loop l en x = match l with
		| [] -> en
		| head::next::tail when head = next -> loop  (next::tail) en (x + 1)
    |	head::tail -> loop tail (en @ [(x, head)]) 1

		in
		loop l [] 1


let () =
	let p elem = match elem with
	| (x,y) -> print_int x ; print_string " " ; print_endline y
	in
	List.iter p (encode ["42"; "ok"; "ok"; "42"])
