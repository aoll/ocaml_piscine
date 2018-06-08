(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: alex <alex@student.42.fr>                  +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/07 22:42:00 by alex              #+#    #+#             *)
(*   Updated: 2018/06/08 00:18:15 by alex             ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

 class virtual atom name symbol atomic_number =
  object
    val _name = name
    val _symbol = symbol
    val _atomic_number = atomic_number

    method name = _name
    method symbol = _symbol
    method atomic_number = _atomic_number
    method equals (a:atom) = a#atomic_number = _atomic_number
    method to_string = "name: " ^ _name ^ " symbol: " ^ _symbol
                       ^ " atomic_number: " ^ (string_of_int _atomic_number)
  end

class hydrogen =
  object
    inherit atom "hydrogen" "H" 1
  end

class carbon =
  object
    inherit atom "carbon" "C" 6
  end

class oxygen =
  object
    inherit atom "oxygen" "O" 2
  end

class lithium =
  object
    inherit atom "lithium" "Li" 3
  end

class nitrogen =
  object
    inherit atom  "nitrogen" "N" 7
  end

class fluor =
  object
    inherit atom "fluor" "F" 9
  end
