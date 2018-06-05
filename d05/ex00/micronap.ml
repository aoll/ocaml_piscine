(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 20:12:11 by alex              #+#    #+#             *)
(*   Updated: 2018/06/04 20:26:43 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)
let my_sleep () = Unix.sleep 1

let () =
  if (Array.length Sys.argv) <> 2 then ()
  else
    begin
      let n = try int_of_string (Array.get Sys.argv 1) with
        | _ -> (-1)
      in
      if n <= 0 then ()
      else
        begin
          let i = ref 0 in
          while !i < n do
            my_sleep () ;
            incr i
          done
        end
    end
