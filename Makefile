.PHONY: all compile run test clean
.PHONY: build_plt dialyzer

REBAR=./rebar3

DIALYZER_APPS = asn1 compiler crypto erts inets kernel public_key sasl ssl stdlib syntax_tools tools

all: $(REBAR) compile

compile:
		$(REBAR) compile

run:
		erl -pa _build/default/lib/*/ebin -boot start_sasl

test:
		$(REBAR) ct skip_deps=true verbose=3

clean:
		$(REBAR) clean
		rm -rf ./test/*.beam
		rm -rf ./erl_crash.dump
		rm -rf TEST*.xml

build_plt: clean compile
ifneq ("$(wildcard erlang.plt)","")
		@echo "Erlang plt file already exists"
else
		dialyzer --build_plt --output_plt erlang.plt --apps $(DIALYZER_APPS)
endif
ifneq ("$(wildcard doppler.plt)","")
		@echo "doppler plt file already exists"
else
		dialyzer --build_plt --output_plt doppler.plt _build/default/lib/*/ebin
endif

add_to_plt: build_plt
		dialyzer --add_to_plt --plt erlang.plt --output_plt erlang.plt.new --apps $(DIALYZER_APPS)
		dialyzer --add_to_plt --plt doppler.plt --output_plt doppler.plt.new _build/default/lib/*/ebin
		mv erlang.plt.new erlang.plt
		mv doppler.plt.new doppler.plt

dialyzer:
		dialyzer --src src --plts erlang.plt doppler.plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

