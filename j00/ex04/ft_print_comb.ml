(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 18:29:39 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 15:21:24 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
  let is_3_diff_elem x y z is_next_result =
    if x <> y && x <> z && y <> z && x < y && y < z then
      begin
        if is_next_result <> 0 then print_string ", " ;
        print_int x ; print_int y ; print_int z ; 1
      end
    else if is_next_result <> 0 then is_next_result
    else
      0
  in
  let rec process_digits x y z is_next_result =
    if x > 9 then print_string "\n"
    else
      begin
        let tmp = is_3_diff_elem x y z is_next_result in
        if  z + 1 < 10 then process_digits x y (z + 1) tmp
        else if y + 1 < 10 then process_digits x (y + 1) 0 tmp
        else process_digits (x + 1) 0 0 tmp
      end
  in
  process_digits 0 0 0 0

let () = ft_print_comb ()
