--with IO
--with Env
--with Iter
with slice

type Vec<?T> = struct of
	content: Slice<?T>,
	len: uint

def Vec<?T> =
	let get of self, idx =
		return get(self.content, idx)
end

type Result<?T, ?E> = enum of
	Ok(?T),
	Err(?E)

type Config = struct of 
	query: str,
	file_path: str,
	ignore_case: bool

def Config =
	let build of args: Vec<str> -> Result<Config, str> =
		if args.len < 3 then
			return Err("Not enough arguments!")
		else 
			query = args.acc(1)
			file_path = args.acc(2)
			ignore_case = env:var("IGNORE_CASE") & is_ok()
			Ok(Config {query = query, file_path = file_path, ignore_case = ignore_case})

	let run of self -> Result<naught, str> =
		contents = IO:read_file(self.file_path) & unwrap()
		res = 
			if self.ignore_case then 
				search_case_insensitive(self.query, contents) 
			else 
				search(self.query, contents)
			& Iter:foreach(fn -> IO:out(line))
		Ok()
end

let search of query: str, contents: str -> Vec<str> =
	contents.lines() & Iter:filter(fn of line -> line.contains(query))

let search_case_insensitive of query: str, contents: str -> Vec<str> =
	q = query.to_lowercase()
	contents.lines() & Iter:filter(fn of line -> line.to_lowercase() & contains(q))

let main =
	args = Env:read_argsv()
	config = Config:build(args) & unwrap_else(
		fn of e -> IO:quit("Problem parsing arguments: " + e, 1)
	)
	if config & run() is Err(e) then IO:quit("Application error: " + e, 1) else IO:exit(0)