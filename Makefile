REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

.PHONY: all compile clean test doc

all: compile

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean
	@rm -rf doc ebin

test: compile
	@$(REBAR) eunit
