(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 04:43:04 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 16:37:04 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec iter f x n =
	if n == 0 then x
    else if n < 0 then -1
    else iter f (f x) (n - 1)


let () =
  print_string "iter (fun x -> x * x) 2 4: " ; print_int (iter (fun x -> x * x) 2 4) ; print_string "\n" ;
  print_string "(iter (fun x -> x * 2) 2 4): " ; print_int (iter (fun x -> x * 2) 2 4) ; print_string "\n"
