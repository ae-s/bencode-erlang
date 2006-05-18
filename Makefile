
EUNITPATH=/home/ssmith/software/eunit-1.1

ERLC=erlc
ERLFLAGS=-I$(EUNITPATH)/inc 

SRC=bencode.erl
TESTSRC=tests.erl
BEAM=$(SRC:.erl=.beam)
TESTBEAM=$(TESTSRC:.erl=.beam)
ALLBEAM=$(BEAM) $(TESTBEAM)

all: $(BEAM)

%.beam : %.erl
	$(ERLC) $(ERLFLAGS) $<

test: $(ALLBEAM)
	erl -noshell -pa $(EUNITPATH)/ebin -s tests -s init stop

clean:
	rm -f $(ALLBEAM) *~
