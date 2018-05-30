(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 19:05:04 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 20:34:11 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi delta =
  if  delta <= 0.  then -1
  else
    begin
      let f i =
        let q =  (if (i mod 2) > 0 then -1. else 1.) in
        4. *. (q /. (2. *. (float_of_int i) +. 1.))
      in
      let d = (4. *. (atan 1.)) in
      let rec loop res i =
        let new_delta = d -. res in
        if  (if new_delta < 0. then new_delta *. (-1.) else new_delta)<= delta then i
        else loop (res +. (f   i))  (i + 1)
      in
      loop 0. 0
    end



let () =
  print_string "0.01: " ; print_int (leibniz_pi 0.01) ; print_string "\n" ;
  print_string "1.0: " ; print_int (leibniz_pi 1.0) ; print_string "\n" ;
  print_string "0.1: " ; print_int (leibniz_pi 0.1) ; print_string "\n" ;
  print_string "0.2: " ; print_int (leibniz_pi 0.2) ; print_string "\n" ;
  print_string "0.3: " ; print_int (leibniz_pi 0.3) ; print_string "\n" ;
  print_string "0.4: " ; print_int (leibniz_pi 0.4) ; print_string "\n" ;
  print_string "0.5: " ; print_int (leibniz_pi 0.5) ; print_string "\n" ;
  print_string "0.6: " ; print_int (leibniz_pi 0.6) ; print_string "\n" ;
  print_string "0.7: " ; print_int (leibniz_pi 0.7) ; print_string "\n" ;
  print_string "0.8: " ; print_int (leibniz_pi 0.8) ; print_string "\n" ;
  print_string "0.9: " ; print_int (leibniz_pi 0.9) ; print_string "\n"
