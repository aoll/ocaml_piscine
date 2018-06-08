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
new Alkane.methane ;
new Alkane.ethane ;
new Alkane.octane ;
]
in
let p elem =
	print_endline elem#to_string
in
List.iter p a ;
print_endline (string_of_bool ((new Alkane.methane)#equals (new Alkane.methane)));
print_endline (string_of_bool ((new Alkane.methane)#equals (new Alkane.ethane)))
