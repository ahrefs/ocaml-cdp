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
test: ## Run all tests (cram + unit + runtime)
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
generate: ## Regenerate lib/ and the roundtrip test from the vendored protocol JSON
	$(DUNE) exec gen/gen.exe -- generate \
	  protocol/browser_protocol.json protocol/js_protocol.json lib $(DOMAINS)
	$(DUNE) exec gen/gen.exe -- roundtrip \
	  protocol/browser_protocol.json protocol/js_protocol.json test/roundtrip/test_roundtrip.ml $(DOMAINS)
	$(MAKE) fmt

.PHONY: update-protocol
update-protocol: ## Fetch the protocol (latest, or REV=1680125) and regenerate
	$(DUNE) exec gen/gen.exe -- fetch protocol $(REV)
	$(MAKE) generate

.PHONY: check
check: ## Fail if lib/ or the roundtrip test does not match the generator output (for CI)
	@rm -rf _check && mkdir _check
	@$(DUNE) exec gen/gen.exe -- generate \
	  protocol/browser_protocol.json protocol/js_protocol.json _check $(DOMAINS) > /dev/null
	@$(DUNE) exec gen/gen.exe -- roundtrip \
	  protocol/browser_protocol.json protocol/js_protocol.json _check/test_roundtrip.ml $(DOMAINS) > /dev/null 2>&1
	@$(FMT_BIN) --inplace _check/*.ml
	@for f in _check/*; do \
	  base=$$(basename $$f); \
	  case $$base in \
	    test_roundtrip.ml) target=test/roundtrip/$$base;; \
	    *) target=lib/$$base;; \
	  esac; \
	  cmp -s "$$f" "$$target" \
	    || { echo "STALE: $$target differs — run 'make generate'"; exit 1; }; \
	done
	@rm -rf _check
	@echo "generated code is fresh"

FULL_CHECK_DIR ?= /tmp/cdp-check-full

.PHONY: check-full
check-full: ## Generate ALL protocol domains into a throwaway project and compile them
	@rm -rf $(FULL_CHECK_DIR) && mkdir -p $(FULL_CHECK_DIR)/lib
	@cp protocol/browser_protocol.json protocol/js_protocol.json protocol/REVISION $(FULL_CHECK_DIR)/
	@printf '(lang dune 3.16)\n' > $(FULL_CHECK_DIR)/dune-project
	@cp lib/cdp_json.ml lib/cdp_command.ml lib/cdp_event.ml lib/cdp_envelope.ml $(FULL_CHECK_DIR)/lib/
	@printf '(library\n (name cdp)\n (wrapped false)\n (libraries melange-json-native yojson)\n (preprocess\n  (pps melange-json-native.ppx ppx_deriving.show ppx_deriving.eq ppx_deriving.make))\n (flags (:standard -w -a -alert -all)))\n' > $(FULL_CHECK_DIR)/lib/dune
	$(DUNE) exec gen/gen.exe -- generate \
	  $(FULL_CHECK_DIR)/browser_protocol.json $(FULL_CHECK_DIR)/js_protocol.json $(FULL_CHECK_DIR)/lib all
	cd $(FULL_CHECK_DIR) && $(DUNE) build
	@rm -rf $(FULL_CHECK_DIR)
	@echo "all protocol domains generate and compile"

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
