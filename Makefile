# Variables
DUNE = opam exec --switch=5.4.1+afl -- dune
FUZZ = ./_build/default/test/fuzz.exe

# List all targets here
TARGETS = setup build fuzz dump kill clean

.PHONY: all help $(TARGETS)

all: build

help:
	@echo "Available make commands:"
	@echo "------------------------"
	@for target in $(TARGETS); do \
		echo "  make $$target"; \
	done

# Catch-all rule: if you type a typo, it runs 'help' instead of breaking
%:
	@echo "Error: Unknown command '$@'\n"
	@$(MAKE) -s help
	@exit 1

setup:
	@echo "Configuring environment variables..."
	@grep -q "AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES" ~/.zshrc || echo 'export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1' >> ~/.zshrc
	@grep -q "AFL_SKIP_CPUFREQ" ~/.zshrc || echo 'export AFL_SKIP_CPUFREQ=1' >> ~/.zshrc
	@echo "Please run 'sudo afl-system-config' manually if prompted."
	sudo afl-system-config

build:
	$(DUNE) build test/fuzz.exe --instrument-with afl

fuzz: build
	@mkdir -p input
	export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_POST_PROCESS_KEEP_ORIGINAL=1; \
	export AFL_MAP_SIZE=256000; \
	afl-fuzz -t 50 -P explore -x test/hs_code.dict -i input -o output -S cpu2 $(FUZZ) > /dev/null 2>&1 & \
	afl-fuzz -t 50 -P explore -x test/hs_code.dict -i input -o output -M main $(FUZZ)

status:
	@if [ -d output ]; then afl-whatsup output/; else echo "No active fuzzing session found."; fi

dump:
	@echo "Dumping and validating test cases..."
	@rm -f output_pass.txt output_fail.txt
	$(DUNE) exec test/check_queue.exe
	@echo "Done. Results split into output_pass.txt and output_fail.txt"

kill:
	@echo "Killing any lingering afl-fuzz instances..."
	-pkill -f afl-fuzz

clean:
	@echo "Cleaning directories..."
	rm -rf output
	$(DUNE) clean
