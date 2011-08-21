
# Makefile for esli
# Artem Golovinsky <artemgolovinsky@gmail.com>

REBAR = ./rebar
REL_ROOT = rel/esli
HTDOCS = $(REL_ROOT)/var/htdocs

all: get-deps compile generate 


get-deps: 
	$(REBAR) get-deps

compile: get-deps
	$(REBAR) compile

generate: get-deps compile
	$(REBAR) generate
	chmod 755 $(REL_ROOT)/bin/esli
	cp -R html/* $(HTDOCS)

clean:
	$(REBAR) clean

clean-all: clean
	$(REBAR) delete-deps