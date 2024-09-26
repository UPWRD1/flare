with "examples/player"

type Loc = struct of 
	x: int,
	y: int

type GameState = struct of 
	playerpos: Loc,
	difficulty: uint

let main =
	mut p = Player::new("joe", Mage(5))
	p.level_up()
	p:level