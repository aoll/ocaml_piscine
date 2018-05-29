(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 17:39:46 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 15:19:16 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () =
  let ascii_of_a = int_of_char 'a' in
  let ascii_of_z = int_of_char 'z' in
  let rec loop ascii_current_char =
    if ascii_current_char <= ascii_of_z then
      let char_of_ascii_current_char = char_of_int ascii_current_char in
      print_char char_of_ascii_current_char ;
      loop (ascii_current_char + 1)
  in
  loop ascii_of_a ;
  print_char '\n'

let () = ft_print_alphabet ()
