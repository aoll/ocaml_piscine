(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 18:30:13 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 18:42:13 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ackermann m n =
  if m = 0 then n + 1
  else if m > 0 && n == 0 then ackermann (m - 1) 1
  else if m > 0 && n > 0 then ackermann (m - 1) (ackermann m (n - 1))
  else
    -1

let () =
  print_int (ackermann 4 1) ; print_string "\n"
