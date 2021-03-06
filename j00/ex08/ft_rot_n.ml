(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 23:17:23 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 16:51:37 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n gape str =
  let new_gape =
    if gape >= 26 then
      gape - ((gape / 26) * 26)
    else
      gape
  in
  let lower_a = int_of_char 'a' in
  let lower_z = int_of_char 'z' in
  let upper_a = int_of_char 'A' in
  let upper_z = int_of_char 'Z' in
  let update_with_gape target start limit =
    if (target + new_gape) > limit then
      char_of_int (start + target + new_gape - limit - 1)
    else
        char_of_int ( target + new_gape )
   in
   let update_char src_char =
     let src_int = (int_of_char src_char) in
      if src_int >= lower_a && src_int <= lower_z then
        update_with_gape src_int lower_a lower_z
      else if src_int >= upper_a && src_int <= upper_z then
        update_with_gape src_int upper_a upper_z
      else
        src_char
  in
  String.map update_char str



let main () =
  print_endline "1 azw! : "; print_endline (ft_rot_n 1 "azw!") ;
  print_endline "260 azw! : "; print_endline (ft_rot_n 260 "azw!") ;
  print_endline "1 abcdefghijklmnopqrstuvwxyz : "; print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") ;
  print_endline "13 abcdefghijklmnopqrstuvwxyz : "; print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") ;
  print_endline "42 0123456789 : "; print_endline (ft_rot_n 42 "0123456789") ;
  print_endline "2 OI2EAS67B9 : "; print_endline (ft_rot_n 2 "OI2EAS67B9";) ;
  print_endline "0 Damned ! : "; print_endline (ft_rot_n 0 "Damned !") ;
  print_endline "42 '' : "; print_endline (ft_rot_n 42 "") ;
  print_endline "1 NBzlk qnbjr ! : "; print_endline ( ft_rot_n 1 "NBzlk qnbjr !")

let () = main ()
