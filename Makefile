EBIN_DIR = ebin
EBIN_DIRS = $(EBIN_DIR) $(wildcard deps/*/ebin)
EPA=-pa $(shell echo $(EBIN_DIRS) | sed 's/ / -pa /')
ERL=erl $(EPA) -boot start_sasl -sasl errlog_type error
REBAR=./rebar

all: compile

shell: compile
	$(ERL)

compile: 
	$(REBAR) compile

deps:
	$(REBAR) get-deps
	$(REBAR) update-deps

test: compile
	$(REBAR) eunit

clean: 
	$(REBAR) clean

doc: compile
	$(REBAR) doc

.PHONY: test deps