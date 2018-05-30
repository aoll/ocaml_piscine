(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 02:13:14 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 04:23:09 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let fibonacci n =
  let rec fib n a b =
    if n > 0 then fib (n - 1) b (a + b)
    else if n < 0 then -1
    else a
  in
  fib n 0 1

let () =
  print_string "-42: " ; print_int (fibonacci (-42)) ; print_char '\n' ;
  print_string "1: " ; print_int (fibonacci 1) ; print_char '\n' ;
  print_string "2: " ; print_int (fibonacci 2) ; print_char '\n' ;
  print_string "3: " ; print_int (fibonacci 3) ; print_char '\n' ;
  print_string "8: " ; print_int (fibonacci 8) ; print_char '\n' ;
  print_string "6: " ; print_int (fibonacci 6) ; print_char '\n' ;
  print_string "20: " ; print_int (fibonacci 20) ; print_char '\n'
