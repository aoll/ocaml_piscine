class virtual reaction
	(start_list : (Molecule.molecule * int) list)
	(result_list : (Molecule.molecule * int) list) =
	object
		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list

		method virtual balance : reaction
		method virtual is_balanced : bool

		 	(* let rec loop l acc = match l with
		 	| (m,n)::t -> loop t (acc +  (m#count_atom) * n)
		 	| [] -> acc
			in
			let count_start = loop start_list 0 in
			let count_result = loop result_list 0 in
			count_start = count_result *)
	end
