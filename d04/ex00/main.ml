let () =
	let s = Color.Spade in
	let h = Color.Heart in
	let d = Color.Diamond in
	let c = Color.Club in
	print_endline ((Color.toString s) ^ " " ^ (Color.toStringVerbose s)) ;
	print_endline ((Color.toString h) ^ " " ^ (Color.toStringVerbose h)) ;
	print_endline ((Color.toString d) ^ " " ^ (Color.toStringVerbose d)) ;
	print_endline ((Color.toString c) ^ " " ^ (Color.toStringVerbose c)) ;
	let p s =
		print_endline ((Color.toString s) ^ " " ^ (Color.toStringVerbose s)) in
	List.iter p Color.all
