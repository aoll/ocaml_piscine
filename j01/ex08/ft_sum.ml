(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 18:42:09 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 18:56:49 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f i n =
  let rec sum acc f i n =
    if i > n then acc
    else sum ((f i) +. acc) f (i + 1) n
  in
  sum 0. f i n

let () =
  print_string "ft_sum (fun i -> float_of_int (i * i)) 1 10: " ; print_float ( ft_sum (fun i -> float_of_int (i * i)) 1 10)
