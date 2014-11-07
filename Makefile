ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

ERL=erl

REBAR=./rebar

.PHONY: all compile doc clean test deps dialyzer typer shell distclean pdf \
	update-deps

all: deps compile

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc:
	$(REBAR) skip_deps=true doc

test: clean all
	$(REBAR) ct skip_deps=true
	make dialyzer	

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
		 --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

shell: deps compile
	# - @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

clean:
	$(REBAR) clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps
