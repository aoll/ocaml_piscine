
let rec caesar n s  =
	String.map (fun c -> char_of_int (((int_of_char c) + n) mod 127) ) s

let rec rot42 s =
	String.map (fun c -> char_of_int (((int_of_char c) + 42) mod 127) ) s

let rec xor k s =
	String.map (fun c -> char_of_int  ((int_of_char c) lxor k ) ) s


let ft_crypt (s:string) fl =
	let rec loop f s = match f with
	| [] -> s
	| head::tail ->  loop tail (head s)
	in
	loop fl s




let _ =
	(* let s1 = (caesar 129 "abc" ) in
	print_endline s1 ;
	print_endline (Uncipher.uncaesar 129 s1) ;
	let s2 = (rot42 "ABC") in
	print_endline s2 ; print_endline (Uncipher.unrot42 s2) ; *)


	let s4 = (ft_crypt "ABC" [(xor 2);(rot42); (caesar 129  ) ]  ) in
	print_endline s4 ;
	print_endline (Uncipher.ft_uncrypt s4 [(xor 2);(Uncipher.unrot42); (Uncipher.uncaesar 129 )]  ) ;
	(* let s3 = (xor 142 "ABCcd" ) in
	print_endline s3 ; print_endline (Uncipher.xor 142 s3 ) *)
