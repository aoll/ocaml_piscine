(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 22:15:18 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 04:55:26 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let count_char s c =
  let i = ref 0 in
  let count = ref 0 in
  let len = (String.length s) in
  while !i < len do
    if (String.get s !i) = c then incr count else () ;
    incr i
  done ;
  !count

let () =
  if (Array.length Sys.argv) <> 2 then ()
  else
    begin
      Random.self_init ();
      let ic = open_in (Array.get Sys.argv 1) in
      let len = (in_channel_length ic) in
      let s = really_input_string ic (len - 1) in
      close_in ic;
      let count = count_char s ';' in
      let arr = Array.make count "" in
      let i = ref 0 in
      let start_sub = ref 0 in
      let end_sub = ref 0 in
      while !i < count do
        end_sub := (String.index_from s !start_sub ';') ;
        Array.set arr !i (String.sub s !start_sub (!end_sub - !start_sub)) ;
        start_sub := (!end_sub + 1);
        incr i
      done ;
      print_endline arr.(Random.int count)
    end
