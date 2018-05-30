(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 18:19:24 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 20:37:11 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?(str="x") n =
  if n < 0 then "Error"
  else
    begin
      let rec loop_x dst s x =
        if x == 0 then dst
        else
          loop_x (dst ^ s) s (x - 1)
      in
      loop_x "" str n
    end

let () =
  print_string ("-1 : " ^ (repeat_string (-1)) ^ "\n") ;
  print_string ("0 : " ^ (repeat_string 0) ^ "\n") ;
  print_string ("Toto 1 : " ^ (repeat_string ~str:"Toto" 1) ^ "\n") ;
  print_string ("2 : " ^ (repeat_string 2) ^ "\n") ;
  print_string ("a 5 : " ^ (repeat_string ~str:"a" 5) ^ "\n")
  
