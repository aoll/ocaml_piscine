(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 04:23:33 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 04:39:35 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n =
  if n == 0 then 1
  else if n < 0 then -1
  else
    n - hfs_m (hfs_f (n - 1))
and hfs_m n =
  if n == 0 then 0
  else if n < 0 then -1
  else
    n - hfs_f(hfs_m(n - 1))

let () =
  print_string "hfs_m (-1) " ; print_int (hfs_m (-1)) ; print_string "\n" ;
  print_string "hfs_f (-1) " ; print_int (hfs_f (-1)) ; print_string "\n" ;
  print_string "hfs_m 0 " ; print_int (hfs_m 0) ; print_string "\n" ;
  print_string "hfs_f 0 " ; print_int (hfs_f 0) ; print_string "\n" ;
  print_string "hfs_m 4 " ; print_int (hfs_m 4) ; print_string "\n" ;
  print_string "hfs_f 4 " ; print_int (hfs_f 4) ; print_string "\n"
