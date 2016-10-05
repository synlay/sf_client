ifeq ($(CI),true)
	CI_ENV = true
else
	CI_ENV = false
endif

REBAR=rebar3

all: compile test dialyze

compile:
	@$(REBAR) compile

typer:
	typer --plt $(DEPS_PLT) -r ./src

pdf:
	pandoc README.md -o README.pdf

ifeq ($(BUILD_ON_CI_ENV),true)
test:
	@ERL_FLAGS="-s lager -config $(CURDIR)/priv/app.config -sasl errlog_type _" $(REBAR) eunit
else
test:
	@ERL_FLAGS="-s lager -config $(CURDIR)/priv/app.config -sasl errlog_type _" $(REBAR) as dev_console eunit
endif

dialyze:
	@echo starting static code analysis with dialyzer...
	@$(REBAR) as test dialyzer

distclean:
	@rm -rf _build

rebuild: distclean all

ci: test dialyze

travis_ci: ci coveralls

coveralls:
	@$(REBAR) as test coveralls send

rel:
	@$(REBAR) as prod release

.PHONY: compile typer pdf test distclean rel dialyze ci coveralls travis_ci
