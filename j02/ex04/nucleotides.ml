type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
	phosphate : phosphate;
	deoxyribose : deoxyribose;
	nucleobase : nucleobase
}

let generate_nucleotide s =
	{
		phosphate = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase = match s with
		| 'A' -> A
		| 'T' -> T
		| 'C' -> C
		| 'G' -> G
		| _ -> None
	}

let () =
	print_string "hello i am a  nucleotide with A and my phosphate is " ;
	let t = generate_nucleotide 'A' in print_endline t.phosphate
