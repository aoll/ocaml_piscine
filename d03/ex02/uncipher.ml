
let rec uncaesar n s =
	let modu x y =
		let n = x mod y in
		if	n >= 0 then n
		else n + y
	in
	String.map (fun c -> char_of_int ( modu (( int_of_char c ) - n )  127)   ) s

let rec unrot42 s =
let modu x y =
	let n = x mod y in
	if	n >= 0 then n
	else n + y
in
	String.map (fun c -> char_of_int ( modu ((int_of_char c) - 42) 127) ) s

	let ft_uncrypt (s:string) fl =
		let rec loop f s = match f with
		| [] -> s
		| head::tail -> loop tail (head s)
		in
		loop fl s

let rec xor k s =
	String.map (fun c -> char_of_int  ( (int_of_char c) lxor k ) ) s
