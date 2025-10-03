#! python3
import sys
import subprocess
from typing import List

def run_args(cmd: List[str]):
    try:
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)

        while True:
            line = process.stdout.readline()
            if not line and process.poll() is not None:
                break
            if line:
                print(line.rstrip(), flush=True) # Print and immediately flush the output

    # After the loop, check for any remaining stderr output
        stderr_output = process.stderr.read()
        if stderr_output:
            print(f"Error output:\n{stderr_output}", file=sys.stderr)

        process.wait() # Wait for the subprocess to finish

    except FileNotFoundError:
        print(f"Error: Command '{cmd[0]}' not found.", file=sys.stderr)
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)

if len(sys.argv) == 2:
    if sys.argv[1] == "b":
            # cargo bench -p flare_internals
        run_args(["cargo", "bench", "-p", "flare_internals"])
    elif sys.argv[1] == "o":
        # cargo run --profile release -p flarec -- -c examples/ntest.flr
        run_args(["cargo", "run", "--profile" "release", "-p", "flarec", "--", "-c", "examples/ntest.flr"])
else: 
    run_args(["cargo", "run", "-p", "flarec", "--", "-c", "examples/ntest.flr"])


