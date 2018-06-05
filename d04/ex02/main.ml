let () =
	let card = Card.newCard Card.Value.Jack Card.Color.Diamond in

	(* let p card = *)
	 print_endline (Card.toString card) ;
	 print_endline (Card.toStringVerbose card) ;
	 print_endline (string_of_bool (Card.isDiamond card))

	 (* in *)
	 (* List.iter p c *)
