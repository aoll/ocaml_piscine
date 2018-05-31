let crossover l1 l2 =
	let rec is_in_list l elem = match l with
	| [] -> false
	| head::tail when head = elem -> true
	| head::tail -> is_in_list tail elem
	in
	let rec loop l acc = match l with
	| [] -> acc
	| head::tail when is_in_list l2 head -> loop tail (acc @ [head])
	| head::tail -> loop tail acc
	in
	loop l1 []

let () =
	let p a =
		print_int a ; print_string "\n"
		in
	let ps s =
		print_string s ; print_string "\n"
		in
	List.iter p (crossover [5;42;1;3] [1;2;3]) ;
	List.iter p (crossover [5;42;1;3] []) ;
	List.iter p (crossover [] [1;2;3]) ;
	List.iter p (crossover [] []) ;
	List.iter ps (crossover ["ok"; "hello"] ["ok"]) 
