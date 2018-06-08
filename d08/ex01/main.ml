(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 00:20:49 by alex              #+#    #+#             *)
(*   Updated: 2018/06/08 00:20:55 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
let a = [
new Molecule.water ;
new Molecule.carbon_dioxyde ;
new Molecule.carbon_monoxyde ;
new Molecule.dioxygen ;
new Molecule.dihydrogen ;
new Molecule.soot ;
]
in
let p elem =
	print_endline elem#to_string
in
List.iter p a ;
print_endline (string_of_bool ((new Molecule.water)#equals (new Molecule.water)));
print_endline (string_of_bool ((new Molecule.water)#equals (new Molecule.carbon_dioxyde)))
