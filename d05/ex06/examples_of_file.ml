(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 04:31:46 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 22:47:40 by alex             ###   ########.fr       *)
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

let get_set s =
  let count = count_char s ',' in
  let arr = Array.make (count) 0. in
  let i = ref 0 in
  let start_sub = ref 0 in
  let end_sub = ref 0 in
  while !i < count  do
    end_sub := (String.index_from s !start_sub ',') ;
    let new_s = (String.sub s !start_sub (!end_sub - !start_sub)) in
      Array.set arr !i (float_of_string new_s) ;
    start_sub := (!end_sub + 1);
    incr i
  done ;
  let class_s = (String.sub s !start_sub ((String.length s)  - !start_sub)) in
  (arr, class_s)

let examples_of_file path =
  let ic = open_in path in
  let rec loop list_point =
      begin
        try
          let new_s = (input_line ic) in
          loop (list_point @ [(get_set new_s)])
        with
        | _ -> list_point
      end
  in
  loop []



let () =
  if (Array.length Sys.argv) = 2 then
    let l = examples_of_file (Array.get Sys.argv 1) in
    let p elem = match elem with
      | (x,y) ->
        begin
          print_string "([|" ;
          let len = ((Array.length x) - 1) in
          for i = 0 to len do
            print_float x.(i);
            if i + 1 < len then print_string ", "
          done ;
          print_string "|], " ;
          print_string (y ^ ")\n")
        end
    in
    List.iter p l
