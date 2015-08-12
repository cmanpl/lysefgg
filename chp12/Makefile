ERLC=/opt/local/bin/erlc
ERLCFLAGS=-o
SRCDIR=.
BEAMDIR=./ebin

all: 
	mkdir -p $(BEAMDIR)
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl

clean: 
	rm -rf $(BEAMDIR)
	rm -rf erl_crash.dump
	rm -f *.beam
