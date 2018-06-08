(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 14:37:26 by alex              #+#    #+#             *)
(*   Updated: 2018/06/08 15:04:13 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


class virtual molecule name (atom_list: Atom.atom list) =
  object
    val _name = name


    method name = _name ^ ""
    method formula =
      let rec loop (l: Atom.atom list) formula_s symbole count = match l with
      | [] -> formula_s
      | h::t -> begin
          if h#symbole <> symbole then
            begin
              if count = (-1) then loop t formula_s h#symbole 1
              else
                begin
                  loop t (formula_s ^ symbole ^ (if count > 1 then (string_of_int count)else "" ) ) h#symbole 1
                end
            end
          else
            loop t formula_s symbole (count + 1)
                end
      in
      let sort_assist a b =
        compare a#name b#name
      in
      loop atom_list "" '\n' (-1)
      (* loop (List.sort sort_assist atom_list) "" '\n' (-1) *)


    (* method equals (a:atom) = a#atomic_number = _atomic_number *)
    (* method to_string = "name: " ^ _name ^ " symbol: " ^ _symbol
                       ^ " atomic_number: " ^ (string_of_int _atomic_number) *)
  end

class water =
  object
    inherit molecule "water"  [new Atom.hydrogen;new Atom.hydrogen;new Atom.oxygen]
  end
