(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 22:15:18 by alex              #+#    #+#             *)
(*   Updated: 2018/06/04 22:26:03 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let a = [|"How do you get a squirrel down from a tree?
Pull down your pants and show him your nuts!";"How did the Scotsman find the sheep in the tall grass?
Satisfying!";"Why did Vladimir Putin fail all his tests in school?
Because he was always Russian!";"So a seal walks into a club...";"What do you call someone who points out the obvious?
Someone who points out the obvious."|] in
  Random.self_init ();
  print_endline a.(Random.int 5)
