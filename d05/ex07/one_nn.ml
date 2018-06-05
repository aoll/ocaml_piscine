(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   one_nn.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 08:07:33 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 09:56:47 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = float array * string

type r_list = radar list

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

let get_points t = match t with
  | (points,_) -> points
  | _ -> [||]

let get_label t = match t with
  | (_,label) -> label
  | _ -> ""

let one_nn (list_r:r_list) (testing:radar) =
  let rec loop l near dist = match l with
    | head::tail -> begin
        let dist_tmp = (eu_dist (get_points head) (get_points testing)) in
        if dist < 0. then loop tail head dist_tmp
        else if dist_tmp < dist && dist_tmp > 0. then loop tail head dist_tmp
        else loop tail near dist
      end
    | [] -> get_label near
  in
  loop list_r testing (float_of_string "-1")

let () =
  if (Array.length Sys.argv) = 2 then
    begin
      let success = ref 0 in
      let fail = ref 0 in
      let l_data = examples_of_file (Array.get Sys.argv 1) in
      let rec loop l = match l with
        | head::tail ->
          begin
            let label = (one_nn l_data head) in
            if label = (get_label head) then incr success else incr fail;
            loop tail
          end
        | [] -> print_string ("success: " ^ (string_of_int !success) ^ " fail: " ^ (string_of_int !fail) ^ " percent success: " ^ (string_of_float (( (float_of_int !success )/. (float_of_int (List.length l_data) )) *. 100.)) ^  "\n")
      in
      loop l_data
    end
