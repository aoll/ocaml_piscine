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
  object (self)
    val _name = name


    method name = _name
    method formula =
      let rec loop (l: Atom.atom list) formula_s symbol count = match l with
      | [] -> formula_s ^ symbol ^ (if count > 1 then (string_of_int count) else "")
      | h::t -> begin
          if h#symbol <> symbol then
            begin
              if count = (-1) then loop t formula_s h#symbol 1
              else
                begin
                  loop t (formula_s ^ symbol ^ (if count > 1 then (string_of_int count)else "" ) ) h#symbol 1
                end
            end
          else
            loop t formula_s symbol (count + 1)
                end
      in
      let sort_assist a b = match (a#symbol, b#symbol) with
      | (a,b) when a = b -> 0
      | (_,"C")  -> 1
      | ("C",_)  -> -1
      | (_,"H")  -> 1
      | ("H",_)  -> -1
      | (a,b)  -> compare a b
      in
      loop (List.sort sort_assist atom_list) "" "" (-1)


    method equals (a:molecule) = a#formula = self#formula
    method to_string = "name: " ^ _name
                       ^ " formula: " ^ self#formula
  end

class water =
  object
    inherit molecule "water"  [new Atom.hydrogen;new Atom.hydrogen;new Atom.oxygen]
  end

class carbon_dioxyde =
	object
		inherit molecule "carbon_dioxyde"  [new Atom.carbon;new Atom.oxygen;new Atom.oxygen]
	end

class carbon_monoxyde =
	object
		inherit molecule "carbon_monoxyde"  [new Atom.carbon;new Atom.oxygen]
	end

class dioxygen =
	object
		inherit molecule "dioxygen"  [new Atom.oxygen;new Atom.oxygen]
	end

class dihydrogen =
	object
		inherit molecule "dihydrogen"  [new Atom.hydrogen;new Atom.hydrogen]
	end

class soot =
	object
		inherit molecule "soot"  [new Atom.carbon]
	end
