REBAR = $(CURDIR)/rebar3

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

.PHONY: all
all: $(REBAR) compile

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: run
run:
	$(REBAR) shell

.PHONY: ct
ct: $(REBAR)
	$(REBAR) ct --name 'test@127.0.0.1' --readable true -v -c

.PHONY: cover
cover: ct
	$(REBAR) cover

.PHONY: fmt
fmt: $(REBAR)
	$(REBAR) fmt

.PHONY: fmt-check
fmt-check: $(REBAR)
	$(REBAR) fmt --check

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: clean
clean:
	@rm -rf _build
	@rm -rf rebar3
	@rm -rf *_crash.dump
	@rm -rf ulid_*_plt

.PHONY: dialyzer
dialyzer: $(REBAR)
		$(REBAR) dialyzer

