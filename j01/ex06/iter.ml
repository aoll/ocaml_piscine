(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 04:43:04 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 04:56:46 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
  let rec loop f x n =
    if n = 0 then x
    else  n < 0 then -1
    (* else if n < 0 then -1 *)
    (* else loop (f x) *)
  in
  loop f x n

let () =
  print_string "" ; print_int (iter (fun x -> x * 2) 2 4) ; print_string "\n"
