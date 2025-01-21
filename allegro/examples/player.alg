type Player = struct of 
	name: str,
	level: int,
	hp: int
end

let new for Player of name: str -> Player = 
	return Player {name: name, level: 0, hp: 10}
	
let level_up for Player =
	1
