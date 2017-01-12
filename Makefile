ifeq ($(CI),true)
	CI_ENV = true
else
	CI_ENV = false
endif

# If there is a rebar3 in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR = $(CURDIR)/rebar3
endif

# Fallback to rebar3 on PATH
REBAR ?= $(shell which rebar3)

all: compile test dialyze

compile:
	@$(REBAR) compile

typer:
	typer --plt $(DEPS_PLT) -r ./src

pdf:
	pandoc README.md -o README.pdf

ifeq ($(CI_ENV),true)
test:
	@ERL_FLAGS="-s lager -config $(CURDIR)/priv/app_test.config -sasl errlog_type _" $(REBAR) eunit

cover:
	@$(REBAR) as test cover
else
test:
	@ERL_FLAGS="-s lager -config $(CURDIR)/priv/app_test.config -sasl errlog_type _" $(REBAR) as dev_console eunit

cover:
	@$(REBAR) as dev_console cover
endif

dialyze:
	@echo starting static code analysis with dialyzer...
	@$(REBAR) as test dialyzer

distclean:
	@rm -rf _build

rebuild: distclean all

ci: test dialyzer_concrete

travis_ci: ci coveralls

coveralls:
	@$(REBAR) as test coveralls send

rel:
	@$(REBAR) as prod release

.PHONY: compile typer pdf test distclean rel dialyze dialyzer_concrete ci cover coveralls travis_ci

########################################################################################################################
#                   TEMPORARY REBAR3 Dialyzer performance error workaround based on concrete                           #
########################################################################################################################

ERL=erl
PROJECT=sf_client

DIALYZER = dialyzer
DIALYZER_OPTS = -Wunderspecs -Wrace_conditions -Wunmatched_returns -Werror_handling --statistics
DIALYZER_SKIP_DEPS = merl

# Find all the deps the project has by searching the deps dir
ALL_DEPS = $(notdir $(wildcard _build/default/lib/*))
# Create a list of deps that should be used by dialyzer by doing a
# complement on the sets
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS), $(ALL_DEPS))
# Create the path structure from the dep names
# so dialyzer can find the .beam files in the ebin dir
# This list is then used by dialyzer in creating the local PLT
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),_build/default/lib/$(dep)/ebin)

DEPS_PLT = _build/test/deps.plt

ERLANG_DIALYZER_APPS ?= asn1 \
                        compiler \
                        crypto \
                        edoc \
                        erts \
                        inets \
                        kernel \
                        mnesia \
                        public_key \
                        ssl \
                        stdlib \
                        syntax_tools \
                        tools \
                        xmerl

PROJ ?= $(notdir $(CURDIR))

# Let's compute $(BASE_PLT_ID) that identifies the base PLT to use for this project
# and depends on your `$(ERLANG_DIALYZER_APPS)' list and your erlang version

# As of OTP release 17, the OTP release number corresponds to the major part of the OTP version and the actual current
# OTP version can be read from the text file <OTP installation root>/releases/<OTP release number>/OTP_VERSION or
# <OTP source root>/OTP_VERSION.
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~p", [filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])]), halt().' -noshell | xargs cat 2>/dev/null)

ifeq ($(ERLANG_VERSION),)
# Fallback if OTP is not an installed development system or some error occurred
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~p", [filename:join([code:root_dir(), "OTP_VERSION"])]), halt().' -noshell | xargs cat 2>/dev/null)
endif

ifeq ($(ERLANG_VERSION),)
# Fallback for Erlang/OTP versions < 17 or some error occurred
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell)
endif

MD5_BIN := $(shell which md5 || which md5sum)
ifeq ($(MD5_BIN),)
# neither md5 nor md5sum, we just take the project name
BASE_PLT_ID := $(PROJ)
else
BASE_PLT_ID := $(word 1, $(shell echo $(ERLANG_DIALYZER_APPS) | $(MD5_BIN)))
endif

BASE_PLT := ~/.cache/rebar3/.concrete_dialyzer_plt_$(BASE_PLT_ID)_$(ERLANG_VERSION).plt

DIALYZER_SRC = -r _build/test/lib/$(PROJECT)/ebin

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer_concrete: $(BASE_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DIALYZER_SRC)
else
dialyzer_concrete: $(BASE_PLT) $(DEPS_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DEPS_PLT) $(DIALYZER_SRC)

$(DEPS_PLT):
	@$(DIALYZER) --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

$(BASE_PLT):
	@echo "Missing $(BASE_PLT). Please wait while a new PLT is compiled."
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(BASE_PLT)
	@echo "now try your build again"
