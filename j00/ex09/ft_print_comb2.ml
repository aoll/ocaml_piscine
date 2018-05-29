(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 01:43:21 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 03:26:03 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb2 () =
  let max = 99 in
  let print_p integer =
    if integer < 10 then print_char '0' ;
    print_int integer
  in
  let rec loop x y =
    if x < max then
      if x <> y then
        begin
          print_p x ;
          print_char ' ';
          print_p y;
          if x == 98 && y == 99 then
            print_char '\n'
          else
            begin
              print_char ',' ;
              print_char ' '
            end ;
          if y < max then loop x (y + 1)
          else
            loop (x + 1) (x + 2)
        end
  in
  loop 0 1

let main () =
  ft_print_comb2 ()

let () = main ()
