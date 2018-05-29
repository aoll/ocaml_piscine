(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 16:21:31 by alex              #+#    #+#             *)
(*   Updated: 2018/05/28 16:38:53 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)
let rec ft_countdown x =
  if x <= 0
  then
    begin
      print_int 0 ;
      print_char '\n'
    end
  else
    begin
      print_int x ;
      print_char '\n' ;
      ft_countdown (x - 1)
    end


let main () =
  ft_countdown 42 ;
  ft_countdown 0 ;
  ft_countdown (-42)

let () = main ()
