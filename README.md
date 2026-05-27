# cptpp-fuzz

before commit:
* `dune build @install --auto-promote` - for .opam to be happy
* `opam install . --deps-only --with-test` - install everything
* `make setup` - for my machine, require sudo
* ``

also you need to install AFL


Instead of juggling a bunch of open terminal windows, you can just use these quick shortcuts:

* `make help` – Shows all available commands
* `make setup` – Runs the environment setup (you only need to do this once)
* `make build` – Compiles the fuzzer
* `make fuzz` – Compiles and starts the fuzzer on 4 background cores
* `make status` – Checks how all your targets and cores are doing
* `make output` – Collects all the queue cases into an output.txt file
* `make kill` – Stops the background fuzzers but keeps your data safe
* `make clean` – Stops everything, wipes the results, and cleans up the repo