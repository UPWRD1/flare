
type PlayerClass = enum of
	Barbarian(int),
	Rogue(int),
	Mage(int)

type Player = struct of 
	name: str,
	level: int,
	hp: uint,
	class: PlayerClass


def Player =
	let new of name: str, class: PlayerClass -> Player = 
		return Player {name: name, level: 0, hp: 10, class: class}

	let level_up of mut self =
		-- self = Player {name: self.name, level: self.level + 1, hp: self.hp, class: self.class}
		self.level = self.level + 1

end