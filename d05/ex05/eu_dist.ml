(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 01:50:43 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 03:53:10 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let eu_dist a b =
  (* TODO: check equal len *)
  let n = (Array.length a) in
  let rec loop i acc =
    if i = n then sqrt acc
    else
      begin
        let tmp = (abs_float (a.(i) -. b.(i))) in
        loop (i + 1)  (acc +. (tmp *. tmp))
      end
  in
  loop 0 0.

let () =
  print_string "eu_dist [|1.;10.|] [|2.;-20.|] : " ;
  print_float (eu_dist [|1.;10.|] [|2.;-20.|]) ; print_char '\n';
  print_string "eu_dist [|1.;2.;3.;4.|] [|2.;3.;4.;5.|] : " ;
  print_float (eu_dist [|1.;2.;3.;4.|] [|2.;3.;4.;5.|]) ; print_char '\n'
