(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 16:37:20 by alex              #+#    #+#             *)
(*   Updated: 2018/05/30 17:02:36 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
	let tmp = f (x) in
	if n < 0 then false
    else if  tmp == x then true
    else if n == 0 then false
    else converges f tmp (n - 1)

let () =
  if converges (( * ) 2) 2 5 == true then print_string "true\n"
  else
    print_string "false\n" ;
  if converges (fun x -> x / 2) 2 3 == true then print_string "true\n"
  else
    print_string "false\n" ;
  if converges (fun x -> x / 2) 2 2 then print_string "true\n"
  else
    print_string "false\n"
