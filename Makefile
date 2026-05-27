# Variables
DUNE = opam exec --switch=5.4.1+afl -- dune
FUZZ = ./_build/default/test/fuzz.exe

# List all targets here
TARGETS = setup build fuzz status output kill clean

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

# 1. Machine Environment Setup
setup:
	@echo "Configuring environment variables..."
	@grep -q "AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES" ~/.zshrc || echo 'export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1' >> ~/.zshrc
	@grep -q "AFL_SKIP_CPUFREQ" ~/.zshrc || echo 'export AFL_SKIP_CPUFREQ=1' >> ~/.zshrc
	@echo "Please run 'sudo afl-system-config' manually if prompted."
	sudo afl-system-config

# 2. Compilation
build:
	$(DUNE) build test/fuzz.exe --instrument-with afl

# 3. Running Multi-core Fuzzing safely in the background
fuzz: build
	@echo "Starting 4-core fuzzing for fuzz_sys..."
	@mkdir -p input
	AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 AFL_SKIP_CPUFREQ=1 afl-fuzz -i input -o output -M main $(FUZZ) @@ > /dev/null 2>&1 &
	AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 AFL_SKIP_CPUFREQ=1 afl-fuzz -i input -o output -S cpu2 $(FUZZ) @@ > /dev/null 2>&1 &
	AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 AFL_SKIP_CPUFREQ=1 afl-fuzz -i input -o output -S cpu3 $(FUZZ) @@ > /dev/null 2>&1 &
	AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 AFL_SKIP_CPUFREQ=1 afl-fuzz -i input -o output -S cpu4 $(FUZZ) @@ > /dev/null 2>&1 &
	@echo "Fuzzers running in background. Use 'make status' to monitor."

# 4. Diagnostics & Monitoring
status:
	@if [ -d output ]; then afl-whatsup output/; else echo "No active fuzzing session found."; fi

output:
	@echo "Dumping test cases to output.txt..."
	@rm -f output.txt
	@for f in output/*/queue/id*; do \
		if [ -f "$$f" ]; then \
			echo "=== $$f ===" >> output.txt; \
			cat "$$f" >> output.txt; \
			echo "" >> output.txt; \
			fi; \
	done
	@echo "Done. Open output.txt to view results."

# 5. Teardown and Kill Fuzzers
kill:
	@echo "Killing any lingering afl-fuzz instances..."
	-pkill -f afl-fuzz

clean:
	@echo "Removing output.txt..."
	rm -f output.txt
	@echo "Cleaning directories..."
	rm -rf output
	$(DUNE) clean
