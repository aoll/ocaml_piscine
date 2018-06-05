(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 20:45:16 by alex              #+#    #+#             *)
(*   Updated: 2018/06/04 22:14:42 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable contents:'a;}

let return a =
  {contents = a}

let get t =
  t.contents

let set t a =
  t.contents <- a

let bind t f:('b ft_ref) =
   f (get t)


let () =
  let elem = return 42 in
  print_string "return 42 : " ;
  print_int elem.contents ;
  print_char  '\n';
  print_string "get elem : " ;
  print_int (get elem) ;
  print_char  '\n';
  let b = bind elem (fun x -> return (x * 2)) in
  print_string "bind elem : " ;
  print_int (get b) ;
  print_char  '\n'
