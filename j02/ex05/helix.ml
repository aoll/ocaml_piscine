type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
	phosphate : phosphate;
	deoxyribose : deoxyribose;
	nucleobase : nucleobase
}
type helix = nucleotide list

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


let rec generate_helix n =
	Random.self_init ();
	let map_int_char index = match index with
	| 0 -> 'A'
	| 1 -> 'T'
	| 2 -> 'C'
	| 3 -> 'G'
	| _ -> 'a'
	in
	let l:helix = [] in
	let rec loop h n = match n with
		| n when n = 0 -> h
		| _ -> loop (h @ [(generate_nucleotide (map_int_char (Random.int 4) )  )])  (n - 1)
	in
	loop l n

let helix_to_string h =
	let format t = match t with
		| A -> "A"
		| T -> "T"
		| C -> "C"
		| G -> "G"
		| _ -> ""
	in
	let rec loop l str = match l with
	| [] -> str
	| head::tail -> loop tail (str ^ (format head.nucleobase))
	in
	loop h ""

let complementary_helix h =
	let n:helix = [] in
	let new_n a = match a with
	| a when a.nucleobase = A -> generate_nucleotide 'T'
	| a when a.nucleobase = T -> generate_nucleotide 'A'
	| a when a.nucleobase = C -> generate_nucleotide 'G'
	| a when a.nucleobase = G -> generate_nucleotide 'C'
	| _ -> generate_nucleotide 'a'
	in
	let rec loop l dst = match l with
	| [] -> dst
	| head::tail -> loop tail (dst @ [ (new_n head) ] )

	in
	loop h n

let () =
	let a  = generate_helix 2 in
	print_endline (helix_to_string a) ;
	print_endline (helix_to_string (complementary_helix (a)))
