(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 21:40:26 by alex              #+#    #+#             *)
(*   Updated: 2018/05/29 16:44:01 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_string_all func str =
  let rec loop s index =
    if index >= String.length s then true
    else if (func (String.get s index)) <> true then false
    else loop s (index + 1)
  in
  loop str 0



let main () =
  let is_digit c = c >= '0' && c <= '9' in
    print_string "1234 : " ; print_string (if (ft_string_all is_digit "1234") then  "true\n" else "false\n") ;
    print_string "hello : " ; print_string (if (ft_string_all is_digit "hello") then  "true\n" else "false\n") ;
    print_string " : " ; print_string (if (ft_string_all is_digit "") then  "true\n" else "false\n") ;
    print_string "1H234 : " ; print_string (if (ft_string_all is_digit "1H234") then  "true\n" else "false\n") ;
    print_string "0123456789 : " ; print_string (if (ft_string_all is_digit "0123456789") then  "true\n" else "false\n") ;
    print_string "O12EAS67B9 : " ; print_string (if (ft_string_all is_digit "O12EAS67B9") then  "true\n" else "false\n")

let () = main ()
