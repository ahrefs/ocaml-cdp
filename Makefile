.DEFAULT_GOAL := help

# No local switch yet; override on machines where deps live elsewhere, e.g.:
#   make build DUNE="opam exec --switch <path-to-your-opam-switch> -- dune"
DUNE ?= opam exec -- dune
FMT_BIN ?= opam exec -- ocamlformat

# Domains currently generated (a browser-automation set with its dependencies).
DOMAINS ?= Browser,DOM,Debugger,Emulation,IO,Network,Page,Runtime,Security,Target

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the project
	$(DUNE) build

.PHONY: test
test: ## Run all tests (golden + runtime)
	$(DUNE) runtest

.PHONY: test-promote
test-promote: ## Run tests and promote expected outputs
	$(DUNE) runtest --auto-promote

.PHONY: fmt
fmt: ## Format code with ocamlformat
	$(DUNE) build @fmt --auto-promote

.PHONY: fmt-check
fmt-check: ## Check formatting without modifying files
	$(DUNE) build @fmt

.PHONY: generate
generate: ## Regenerate lib/ from the vendored protocol JSON
	$(DUNE) exec gen/gen.exe -- generate \
	  protocol/browser_protocol.json protocol/js_protocol.json lib $(DOMAINS)
	$(MAKE) fmt

.PHONY: update-protocol
update-protocol: ## Fetch the protocol (latest, or REV=1680125) and regenerate
	$(DUNE) exec gen/gen.exe -- fetch protocol $(REV)
	$(MAKE) generate

.PHONY: check
check: ## Fail if lib/ does not match the generator output (for CI)
	@rm -rf _check && mkdir _check
	@$(DUNE) exec gen/gen.exe -- generate \
	  protocol/browser_protocol.json protocol/js_protocol.json _check $(DOMAINS) > /dev/null
	@$(FMT_BIN) --inplace _check/*.ml
	@for f in _check/*; do \
	  cmp -s "$$f" "lib/$$(basename $$f)" \
	    || { echo "STALE: lib/$$(basename $$f) differs — run 'make generate'"; exit 1; }; \
	done
	@rm -rf _check
	@echo "generated code is fresh"

.PHONY: clean
clean: ## Clean build artifacts
	$(DUNE) clean

.PHONY: doc
doc: ## Build documentation
	$(DUNE) build @doc

.PHONY: opam-lint
opam-lint: ## Lint the opam files
	opam lint cdp.opam cdp-gen.opam

.PHONY: all
all: build test fmt-check opam-lint ## Build, test, check formatting, and lint
