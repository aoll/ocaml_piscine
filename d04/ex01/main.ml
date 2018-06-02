let () =
	let rec print p =
		print_endline ((Value.toString p) ^ " " ^ (Value.toStringVerbose p) ^ " "
		^  (string_of_int (Value.toInt p)) ^ " previous: "
		^   (try Value.toStringVerbose (Value.previous p) with invalid_arg -> "Oups a error append.."  )
		^ " next: " ^ (try Value.toStringVerbose (Value.next p) with invalid_arg -> "Oups a error append.."  ) )


	in
	let l = Value.all in
	List.iter print l
