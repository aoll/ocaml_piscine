
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	(* Graphics.open_graph " 800x600"; *)
	Graphics.moveto (x - (size/2)) (y - (size/2));
	Graphics.lineto (x + (size/2)) (y - (size/2)) ;
	Graphics.lineto (x + (size/2)) (y + (size/2)) ;
	Graphics.lineto (x - (size/2)) (y + (size/2)) ;
	Graphics.lineto (x - (size/2)) (y - (size/2))

let draw_tree_node t =
	let size = 50 in
	let draw n =
		Graphics.open_graph " 800x600";
		draw_square 400 300 size ; Graphics.moveto (400 - 5) (300 - 5) ; Graphics.draw_string (string_of_int n);
		Graphics.moveto (400 ) (300 - (size/2)) ; Graphics.lineto (400 - size) (300 - (size * 2));
		Graphics.moveto (400 ) (300 - (size/2)) ; Graphics.lineto (400 + size) (300 - (size * 2)) ;
		draw_square (400 - size) (300 - (size * 2) - (size / 2)) size ;
		Graphics.moveto (400 - size - 5) (300 - (size * 2) - (size /2) - 5 ) ; Graphics.draw_string "Nil";
		draw_square (400 + size) (300 - (size * 2) - (size / 2)) size ;
		Graphics.moveto (400 + size - 5) (300 - (size * 2) - (size /2) - 5) ; Graphics.draw_string "Nil"
	in
	let draw_tree t = match t with
		| Nil -> ()
		| Node (x,_,_) -> draw x
	in
	draw_tree t

let _ =
	draw_tree_node (Node (42, Nil, Nil)) ;
	ignore (read_line ())
