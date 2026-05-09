```ruby
package Superheroes =
	use Science.Units.{self, Speed, Time, Temperature}

	type Flight = (
		max_speed: Speed,
		max_duration: Time,
	)

	type LaserVision = (
		max_melt_power: Temperature
    )
    
	property Power =
		description: self -> str
		sound_effect: str

	def Power for Flight
		let description self = 
			format(
				"Fly at ", 
				self.max_speed, 
				" for ", 
				self.max_time
			)

		let sound_effect = "Whooosh!"

	def Power for LaserVision =
		let description self = 
			format(
				"Shoot lasers at ", 
				self.max_melt_power, 
				" degrees " 
				Units.current_unit_of(Temperature)
			)

		let sound_effect = "Bzzzzzzzz"
	
	property Superhero =
		get_powers: self -> Seq[impl Power],
	
		let use_powers: self -> Seq[str] =
			let powers = self :: get_powers in 
							# ^^ implicit property invocation
			powers :Iter: map fn power => power :: sound_effect
				#  ^^^^^^ explicit property invocation

struct Superman = 
	powers: Seq[impl Power]

```
```ruby
Superheroes = {
	use Science.Units.{self, Speed, Time, Temperature}
	Power = type {
		description: self -> str
		sound_effect: str
	}

	Flight = type {
		max_speed: Speed,
		max_duration: Time,
	} extend :: Power = {
		description me = 
			format(
				"Fly at ", 
				self.max_speed, 
				" for ", 
				self.max_time
			)
		sound_effect = "Whooosh!"
	}

	LaserVision = type {
		max_melt_power: Temperature
    } 
    
    extend LaserVision :: Power = { 
		description self = 
			format(
				"Shoot lasers at ", 
				self.max_melt_power, 
				" degrees " 
				Sys.Locale.unit_of(Temperature)
			)

		sound_effect = "Bzzzzzzzz"
    }

	Superhero = type {
		get_powers: self -> Seq[impl Power],
	} extend {
		use_powers: self -> Seq[str] =
			let powers = self :: get_powers in 
							# ^^ implicit property invocation
			powers :Iter: map fn power => power :: sound_effect
				#  ^^^^^^ explicit property invocation
	}
}

struct Superman = 
	powers: Seq[impl Power]

```