(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   k_nn.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 10:20:13 by alex              #+#    #+#             *)
(*   Updated: 2018/06/05 13:58:18 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = float array * string

(* type short_radar = float * string *)

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

let get_label t = match t with
  | (_,label) -> label

let get_short_label t = match t with
  | (_,label) -> label

let new_short_radar t count =
  (count, (get_label t))

let set_elem elem = match elem with
  | (count, label) -> ((count + 1), label)


let k_nn (list_r:r_list) n (testing:radar) =
  let compare a b =
    let dist_a = eu_dist (get_points testing) (get_points a) in
    let dist_b = eu_dist (get_points testing) (get_points b) in
    if dist_a = 0. then 1
    else if dist_b = 0. then -1
    else if dist_a > dist_b then 1 else if dist_a < dist_b then -1 else 0
  in
  let l = List.sort compare list_r in

  let sorted_counted_label = ref [||] in

  let is_exist s =
    let is_present = ref false in
    for i = 0 to ((Array.length  !sorted_counted_label) - 1) do
      if (get_short_label !sorted_counted_label.(i)) = s then
        begin
          Array.set !sorted_counted_label i (set_elem !sorted_counted_label.(i));
          is_present := true
        end
    done ;
    if !is_present = false then
      begin
        sorted_counted_label := Array.append !sorted_counted_label [|(0, s)|]
      end
  in

  let rec f ls index = match ls with
    | h::t -> is_exist (get_label h) ; if  (index + 1) < n then f t (index + 1)
    | [] -> ()
  in
  f l 0 ;

  let rec liter i len count label =
    if i >= len then label
    else  match !sorted_counted_label.(i) with
      | (c, ll) ->
        begin
          if c > count then liter (i + 1) len c ll
          else liter (i + 1) len count label
        end
  in
  liter 0 (Array.length !sorted_counted_label) (-1) ""



let () =
  if (Array.length Sys.argv) = 2 then
    begin
      let success = ref 0 in
      let fail = ref 0 in
      let l_data = examples_of_file (Array.get Sys.argv 1) in
      let rec loop l = match l with
        | head::tail ->
          begin
            let label = (k_nn l_data 4 head) in
            if label = (get_label head) then incr success else incr fail;
            loop tail
          end
        | [] -> print_string ("success: " ^ (string_of_int !success) ^ " fail: " ^ (string_of_int !fail) ^ " percent success: " ^ (string_of_float (( (float_of_int !success )/. (float_of_int (List.length l_data) )) *. 100.)) ^  "%\n")
      in
      loop l_data
    end
