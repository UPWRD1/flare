```ruby
package Billing = 
	struct PlateRecord = 
		plate: LicensePlate,
		entrance: LocationID,
		exit: LocationID,
	
	enum LicensePlate = 
		Unidentified{ img: ImageRef },
		USA {String},
		Foreign {IsoCountry.Code, String}
	
	# ...
	
	struct PlateCharge = 
		plate: LicensePlate,
		amount: Money.Amount,
	
	let calc_amount db rec : Database -> PlateRecord -> PlateCharge = 
		let dist = db.distance rec.entrance rec.exit in
		let amount = Money.from (dist * db.rate) in
		PlateCharge {plate: rec.plate, amount }
	
	struct Bill = 
		account: AccountID,
		amount: PlateCharge,
	
	let blame_charge db charge : Database -> PlateCharge -> Bill =
		let account = db.get_acc charge.plate in
		Bill {account, charge.amount}
		
	
	pub let bill_plate db plate : Database -> PlateRecord -> Bill =
		db.blame_charge db.calc_amount plate

package Camera = 
	struct CamData = 
		location: LocationID,
		events: Vec[CameraEvent],
	
	struct CameraEvent = 
		plate: LicensePlate
		time: Time.Stamp
	
	extern "plate_ocr" : ImageRef -> LicensePlate
	
	let process data : Eventual[CamData] -> 
```

```ruby
package Billing = 
	struct PlateRecord of
		plate: LicensePlate,
		entrance: LocationID,
		exit: LocationID,
	
	enum LicensePlate of
		Unidentified{ img: ImageRef },
		USA {String},
		Foreign {IsoCountry.Code, String}
	
	# ...
	
	struct PlateCharge of 
		plate: LicensePlate,
		amount: Money.Amount,
	
	let calc_amount: (db: Database, rec: PlateRecord) -> PlateCharge = 
		dist = distance(rec.entrance, rec.exit)
		amount = Money.from(dist * db.rate)
		PlateCharge {plate: rec.plate, amount }
	
	struct Bill of
		account: AccountID,
		amount: PlateCharge,
	
	let blame_charge: (db: Database, charge: Platecharge) -> Bill =
		account = db.get_acc(charge.plate)
		Bill {account, charge.amount}
		
	
	pub let bill_plate: (db: Database, plate: PlateRecord) -> Bill =
		db.blame_charge(db.calc_amount(plate))

package Camera = 
	struct CamData = 
		location: LocationID,
		events: Vec[CameraEvent],
	
	struct CameraEvent = 
		plate: LicensePlate
		time: Time.Stamp
	
	extern "plate_ocr" : ImageRef -> LicensePlate
	
	let process data : Eventual[CamData] -> 
```

```ruby
package Billing = 
	type PlateRecord = (
		plate: LicensePlate,
		entrance: LocationID,
		exit: LocationID,
		)
	
	type LicensePlate = |
		Unidentified (img: ImageRef ),
		USA(String),
		Foreign(IsoCountry.Code, String)
	|
	# ...
	
	type PlateCharge = (
		plate: LicensePlate,
		amount: Money.Amount,
	)
	
	let calc_amount(db: Database, rec: PlateRecord) -> PlateCharge = 
		dist = distance(rec.entrance, rec.exit)
		amount = Money.from dist * db.rate
		PlateCharge(plate: rec.plate, amount)
	
	type Bill = (
		account: AccountID,
		amount: PlateCharge,
	)
	
	let blame_charge: (db: Database, charge: Platecharge) -> Bill =
		account = db.get_acc(charge.plate)
		Bill {account, charge.amount}
		
	
	pub let bill_plate: (db: Database, plate: PlateRecord) -> Bill =
		db.blame_charge(db.calc_amount(plate))

package Camera = 
	struct CamData = 
		location: LocationID,
		events: Vec[CameraEvent],
	
	struct CameraEvent = 
		plate: LicensePlate
		time: Time.Stamp
	
	extern "plate_ocr" : ImageRef -> LicensePlate
	
	let process data : Eventual[CamData] -> 
	
```
```ruby
package Billing = 
	struct PlateRecord = 
		plate: LicensePlate,
		entrance: LocationID,
		exit: LocationID,
	
	enum LicensePlate = 
		Unidentified{ img: ImageRef },
		USA {String},
		Foreign {IsoCountry.Code, String}
	
	# ...
	
	struct PlateCharge = 
		plate: LicensePlate,
		amount: Money.Amount,
	
	let calc_amount db rec : Database -> PlateRecord -> PlateCharge = 
		let dist = db.distance rec.entrance rec.exit in
		let amount = Money.from (dist * db.rate) in
		PlateCharge {plate: rec.plate, amount }
	
	struct Bill = 
		account: AccountID,
		amount: PlateCharge,
	
	let blame_charge db charge : Database -> PlateCharge -> Bill =
		let account = db.get_acc charge.plate in
		Bill {account, charge.amount}
		
	
	pub let bill_plate db plate : Database -> PlateRecord -> Bill =
		db.blame_charge db.calc_amount plate

package Camera = 
	struct CamData = 
		location: LocationID,
		events: Vec[CameraEvent],
	
	struct CameraEvent = 
		plate: LicensePlate
		time: Time.Stamp
	
	extern "plate_ocr" : ImageRef -> LicensePlate
	
	let process data : Eventual[CamData] -> 
```

# Modern

```ruby
Billing = {
	PlateRecord = type {
		plate: LicensePlate,
		entrance: Location,
		exit: Location,
	}

	LicensePlate = type {
		Unidentified{ img: ImageRef },
		USA {String},
		Foreign {IsoCountry.Code, String}
	|

	# ...

	PlateCharge = type { 
		plate: LicensePlate,
		amount: Money.Amount,
	}
	
	Bill = type {
		account: AccountID,
		amount: PlateCharge,
	}

	extend Database {
		charge db rec : self -> PlateRecord -> PlateCharge = {
			dist = db :: distance(rec.entrance, rec.exit)
			amount = Money :: from(dist * db.rate)
			return PlateCharge {plate = rec.plate, amount }
		}

		blame_charge db charge : self -> Platecharge -> Bill = {
			account = match db :: find_account charge.plate
					as Some acc then acc
					as None then db :: temp_account_for charge.plate
				end
			return Bill {account, charge.amount}
		}

		pub bill_plate self plate : self -> PlateRecord -> Bill = 
			db.charge plate |> db.blame_charge
	}
}

Camera = {
	CamData = type {
		location: LocationID,
		feed: AsyncTimeStream[ImageRef],
	}

	CameraEvent = type {
		plate: LicensePlate
		time: Time.Stamp
	}

	extend Self {
		plate_ocr img = extern "plate_ocr" : ImageRef -> LicensePlate
	
		pub process camera : self -> AsyncStream[CameraEvent] = {
			camera.feed :: map fn {image, timestamp} => {
				plate = self :: plate_ocr image
				return CameraEvent {plate, time = timestamp}
			}
		}
	}
}
