--with IO
--with Env
--with Iter

with "examples/slice"
with "examples/result"

type Config = struct of 
	query: str,
	file_path: str,
	ignore_case: bool

let search of query: str, contents: str -> Slice<str> =
	contents.lines().filter(fn of line -> line.contains(query))

let search_case_insensitive of query: str, contents: str -> Slice<str> =
	q = query.to_lowercase()
	contents.lines().filter(fn of line -> line.to_lowercase().contains(q))

def Config =
	let build -> Result<Config, str> =
		return Ok(Config {query: args.get(1), file_path: "examples/", ignore_case: args.contains("-i")})

	let run of self =
		contents = self.file_path --& unwrap()
		res = 
			if self.ignore_case then 
				search_case_insensitive(self.query, contents) 
			else 
				search(self.query, contents)
			--& foreach(fn -> out(line))
end


let main =
	-- args = Slice.new("Hello")
	config = Config.build(args) & unwrap_else(fn of e -> quit("Problem parsing arguments: " + e, 1))
	if config.run() is Err(e) then quit("Application error: " + e, 1) else exit(0)
	