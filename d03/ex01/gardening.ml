
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	Graphics.moveto (x - (size/2)) (y - (size/2));
	Graphics.lineto (x + (size/2)) (y - (size/2)) ;
	Graphics.lineto (x + (size/2)) (y + (size/2)) ;
	Graphics.lineto (x - (size/2)) (y + (size/2)) ;
	Graphics.lineto (x - (size/2)) (y - (size/2))

let draw_tree_node t =
	let s = 50 in
	let i_x = 400 in
	let i_y = 500 in
	Graphics.open_graph " 800x600";
	let draw n x y size =
		draw_square x y size ; Graphics.moveto (x - 5) (y - 5) ; Graphics.draw_string  n;
	in
	let draw_line x y size =
	 	Graphics.moveto (x ) (y - (size/2)) ; Graphics.lineto (x - size * 2) (y - (size ));
		Graphics.moveto (x ) (y - (size/2)) ; Graphics.lineto (x + size * 2) (y - (size ))
	in
	let rec draw_tree t x y s = match t with
		| Nil -> draw "Nil" x y s
		| Node (n,w,e) -> draw  n x y s; draw_line x y s; draw_tree w (x - s * 2) (y - (s/2) - (s/4) - (s/2)) (s /2) ; draw_tree e (x + s * 2) (y - (s/2) - (s/4) - (s/2)) (s /2)
		| _ -> ()
	in
	draw_tree t i_x i_y s

let _ =
	draw_tree_node (Node ("42", Node ("42", Nil, Nil), Node ("42", Nil, Node ("42", Nil, Nil)))) ;
	ignore (read_line ())
