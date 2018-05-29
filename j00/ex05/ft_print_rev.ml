(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 20:23:59 by alex              #+#    #+#             *)
(*   Updated: 2018/05/28 20:34:46 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop_print str index =
  if index < 0 then print_char '\n'
  else
    begin
      print_char (String.get str index) ;
      loop_print str (index - 1)
    end

let ft_print_rev str =
  loop_print str ((String.length str) - 1)

let main () =
  ft_print_rev "Hello world !"
  ft_print_rev ""

let () = main ()
