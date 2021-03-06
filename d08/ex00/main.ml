(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 22:42:03 by alex              #+#    #+#             *)
(*   Updated: 2018/06/08 00:19:31 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)
let () =
	let a = [
	new Atom.hydrogen ;
	new Atom.carbon ;
	new Atom.oxygen ;
	new Atom.lithium ;
	new Atom.nitrogen ;
	new Atom.fluor ;
	]
	in
	let p elem =
		print_endline elem#to_string
	in
	List.iter p a ;
	print_endline (string_of_bool ((new Atom.hydrogen)#equals (new Atom.hydrogen)));
	print_endline (string_of_bool ((new Atom.hydrogen)#equals (new Atom.carbon)))
