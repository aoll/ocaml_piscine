(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/29 18:03:00 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 18:18:27 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =
  if n < 0 then "Error"
  else
    begin
      let rec loop_x str x =
        if x < 1 then str
        else
          loop_x (str ^ "x") (x - 1)
      in
      loop_x "" n
    end

let () =
  print_string ("-1 : " ^ (repeat_x (-1)) ^ "\n") ;
  print_string ("0 : " ^ (repeat_x 0) ^ "\n") ;
  print_string ("1 : " ^ (repeat_x 1) ^ "\n") ;
  print_string ("2 : " ^ (repeat_x 2) ^ "\n") ;
  print_string ("5 : " ^ (repeat_x 5) ^ "\n")
