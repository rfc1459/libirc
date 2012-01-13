REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

.PHONY: all deps compile clean distclean test doc

all: compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

doc:
	@$(REBAR) skip_deps=true doc

clean:
	@$(REBAR) skip_deps=true clean
	@rm -rf doc ebin

distclean: clean
	@rm -rf deps

test: compile
	@$(REBAR) skip_deps=true eunit
