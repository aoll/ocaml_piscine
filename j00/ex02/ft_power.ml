(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 17:10:33 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 16:36:54 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x y =
  if (y - 1) == 0 then
    x
  else if x == 0 then 0
  else if y == 0 then 1
  else
    x * (ft_power x (y - 1))



let main () =
  print_string "#ft_power 90 1\n" ;  print_int (ft_power 90 1) ; print_char '\n' ;
  print_string "#ft_power 2 4\n" ;  print_int (ft_power 2 4) ; print_char '\n' ;
  print_string "#ft_power 3 0\n" ;  print_int (ft_power 3 0) ; print_char '\n' ;
  print_string "#ft_power 0 5\n" ;  print_int (ft_power 0 5) ; print_char '\n'


let () = main ()
