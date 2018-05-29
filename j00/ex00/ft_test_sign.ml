(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 16:14:18 by alex              #+#    #+#             *)
(*   Updated: 2018/05/28 16:20:36 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign x =
  if  x < 0
  then print_endline "negative"
  else print_endline "positive"

let main () =
  ft_test_sign 42 ;
  ft_test_sign 0 ;
  ft_test_sign (-42)

let () = main ()
