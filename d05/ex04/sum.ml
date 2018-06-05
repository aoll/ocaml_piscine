(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 01:41:51 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 22:41:35 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum x y =
  x +. y

let () =
  print_string "21. 21. = " ; print_float (sum 21. 21.) ; print_char '\n' ;
  print_string "42. 0. = " ; print_float (sum 42. 0.) ; print_char '\n' ;
  print_string "-32. -10. = " ; print_float (sum (-32.) (-10.)) ; print_char '\n' ;
  print_string "-42. 84. = " ; print_float (sum (-42.) (84.)) ; print_char '\n' ;
