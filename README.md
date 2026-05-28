# cptpp-fuzz: informal verification

This is where I post signed binaries (tagged git commit with my signature) and an (in)formal verification certification (printout of AFL output, my seeds, my accepted output and rejected output)

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

## Informal verfication certificate

```sh
         AFL ++4.40c {main} (./_build/default/test/fuzz.exe) [explore]         
┌─ process timing ────────────────────────────────────┬─ overall results ────┐
│        run time : 0 days, 1 hrs, 11 min, 15 sec     │  cycles done : 340   │
│   last new find : 0 days, 0 hrs, 10 min, 7 sec      │ corpus count : 420   │
│last saved crash : none seen yet                     │saved crashes : 0     │
│ last saved hang : none seen yet                     │  saved hangs : 0     │
├─ cycle progress ─────────────────────┬─ map coverage┴──────────────────────┤
│  now processing : 365*22 (86.9%)     │    map density : 0.00% / 0.01%      │
│  runs timed out : 0 (0.00%)          │ count coverage : 3.20 bits/tuple    │
├─ stage progress ─────────────────────┼─ findings in depth ─────────────────┤
│  now trying : havoc                  │ favored items : 84 (20.00%)         │
│ stage execs : 67/450 (14.89%)        │  new edges on : 110 (26.19%)        │
│ total execs : 5.07M                  │ total crashes : 0 (0 saved)         │
│  exec speed : 1256/sec               │  total tmouts : 1 (0 saved)         │
├─ fuzzing strategy yields ────────────┴─────────────┬─ item geometry ───────┤
│   bit flips : 12/104, 11/103, 2/101                │    levels : 13        │
│  byte flips : 1/13, 1/12, 0/10                     │   pending : 0         │
│ arithmetics : 6/783, 0/420, 0/280                  │  pend fav : 0         │
│  known ints : 2/88, 4/366, 1/480                   │ own finds : 375       │
│  dictionary : 3/234, 2/252, 0/0, 0/0               │  imported : 34        │
│havoc/splice : 318/4.99M, 0/0                       │ stability : 100.00%   │
│py/custom/rq : unused, unused, unused, unused       ├───────────────────────┘
│    trim/eff : disabled, 30.77%                     │             [cpu: 70%]
└─ strategy: explore ────────── state: in progress ──┘^C

+++ Testing aborted by user +++
[*] Writing output/main/fastresume.bin ...
[+] fastresume.bin successfully written with 121675564 bytes.
[+] We're done here. Have a nice day!
```