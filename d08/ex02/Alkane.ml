class virtual alkane n =
  object
    inherit Molecule.molecule
			([|"meth";"eth";"prop";"but";"pent";"hex";"hept";"oct";"non";"dec";"undec";"dodec"|].(n - 1) ^ "ane")
			( (Array.to_list (Array.make n new Atom.carbon)) @ (Array.to_list (Array.make (2 * n + 2) new Atom.hydrogen))  )
  end

class methane =
	object
		inherit alkane 1
	end

class ethane =
	object
		inherit alkane 2
	end

class octane =
	object
		inherit alkane 8
	end
