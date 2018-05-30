(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 01:46:52 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 02:12:50 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let tak x y z =
  if  y < x then
    tak (tak (x - 1) y z ) (tak (y - 1) z x) (tak (z - 1) x y)
  else
    z

let () =
  print_endline (Printf.sprintf "tak 1 2 3:  %d" (tak 1 2 3)) ;
  print_endline (Printf.sprintf "tak 5 23 7:  %d" (tak 5 23 7)) ;
  print_endline (Printf.sprintf "tak 9 1 0:  %d" (tak 9 1 0)) ;
  print_endline (Printf.sprintf "tak 1 1 1:  %d" (tak 1 1 1)) ;
  print_endline (Printf.sprintf "tak 0 42 0:  %d" (tak 0 42 0)) ;
  print_endline (
    Printf.sprintf "tak 23498 98734 98776:  %d" (tak 23498 98734 98776))
