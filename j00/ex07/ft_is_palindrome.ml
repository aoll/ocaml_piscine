(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/28 22:21:55 by alex              #+#    #+#             *)
(*   Updated: 2018/05/28 23:16:40 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str =
  let limit = (String.length str) / 2 in
  let rec loop  index =
    let index_end = (String.length str) - index - 1 in
    if index_end <= limit then true
    else if (String.get str index) <> (String.get str index_end) then false
    else loop (index + 1)
  in
  loop 0

let main () =
  print_string (
    if (ft_is_palindrome "kayak") then  "it's true\n" else "Doubt..\n") ;
  print_string (
    if (ft_is_palindrome "CAR") then  "it's true\n" else "Doubt..\n") ;
  print_string (
    if (ft_is_palindrome "ABBA") then  "it's true\n" else "Doubt..\n") ;
  print_string (
    if (ft_is_palindrome "") then  "it's true\n" else "Doubt..\n")

let () = main ()
