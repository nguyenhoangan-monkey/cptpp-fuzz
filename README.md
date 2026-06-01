<h1 align="center">
<img src="https://raw.githubusercontent.com/nguyenhoangan-monkey/cptpp-fuzz/main/docs/assets/cover.jpg" width="750">
</h1>

Photo by [Mahardika Maulana](https://www.flickr.com/photos/96081001@N07/15137625925) (CC BY). Hatsune Miku © Crypton Future Media, Inc. 2007 (CC BY-NC)

----

This is where I post signed binaries (tagged git commit with my signature) and an (in)formal verification certification (printout of AFL output, my seeds, my accepted output and rejected output)

**For this tag, the function is `lib/domain/hs_code.ml` and the interface `lib/domain/hs_code.mli` at [cptpp-calculator.](https://github.com/nguyenhoangan-monkey/cptpp-calculator/tree/main/lib/domain)**

## How to verify `lib/domain/hs_code.ml`

before commit:
* `dune build @install --auto-promote` - for .opam to be happy
* `opam install . --deps-only --with-test` - install everything
* `make setup` - for my machine, require sudo

also you need to install AFL before you can run this. This code uses... ocaml own AFL implementation.

Instead of juggling a bunch of open terminal windows, you can just use these quick shortcuts:

* `make help` – Shows all available commands
* `make setup` – Runs the environment setup (you only need to do this once)
* `make build` – Compiles the fuzzer
* `make fuzz` – Compiles and starts the fuzzer on 2 background cores
* `make dump` – Collects all the queue cases into an output.txt file
* `make kill` – Stops the background fuzzers but keeps your data safe
* `make clean` – Stops everything, wipes the results, and cleans up the repo

Seeds used: check at `test/seeds.txt`

Please check output_fail.txt and output_pass.txt to see whether the parsing makes sense. I halted the test because by this time, output_fail.txt is just outputting garbage masquading as a new execution pattern.


## Informal verification certificate

```sh
         AFL ++4.40c {main} (./_build/default/test/fuzz.exe) [explore]         
┌─ process timing ────────────────────────────────────┬─ overall results ────┐
│        run time : 0 days, 0 hrs, 15 min, 43 sec     │  cycles done : 88    │
│   last new find : 0 days, 0 hrs, 0 min, 5 sec       │ corpus count : 359   │
│last saved crash : none seen yet                     │saved crashes : 0     │
│ last saved hang : none seen yet                     │  saved hangs : 0     │
├─ cycle progress ─────────────────────┬─ map coverage┴──────────────────────┤
│  now processing : 131.84 (36.5%)     │    map density : 0.00% / 0.01%      │
│  runs timed out : 0 (0.00%)          │ count coverage : 2.43 bits/tuple    │
├─ stage progress ─────────────────────┼─ findings in depth ─────────────────┤
│  now trying : havoc                  │ favored items : 119 (33.15%)        │
│ stage execs : 1/50 (2.00%)           │  new edges on : 139 (38.72%)        │
│ total execs : 1.21M                  │ total crashes : 0 (0 saved)         │
│  exec speed : 1143/sec               │  total tmouts : 2 (0 saved)         │
├─ fuzzing strategy yields ────────────┴─────────────┬─ item geometry ───────┤
│   bit flips : 11/264, 6/262, 2/258                 │    levels : 9         │
│  byte flips : 1/33, 0/31, 0/27                     │   pending : 0         │
│ arithmetics : 3/2196, 0/3220, 0/2940               │  pend fav : 0         │
│  known ints : 2/269, 2/1098, 0/1452                │ own finds : 314       │
│  dictionary : 6/1020, 18/1085, 0/0, 0/0            │  imported : 25        │
│havoc/splice : 253/1.20M, 0/0                       │ stability : 100.00%   │
│py/custom/rq : unused, unused, unused, unused       ├───────────────────────┘
│    trim/eff : disabled, 75.76%                     │             [cpu: 64%]
└─ strategy: explore ────────── state: in progress ──┘^C

+++ Testing aborted by user +++
[*] Writing output/main/fastresume.bin ...
[+] fastresume.bin successfully written with 158369807 bytes.
[+] We're done here. Have a nice day!
```
