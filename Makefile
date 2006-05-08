
EUNITPATH=/home/ssmith/software/eunit-1.1

ERLC=erlc
ERLFLAGS=-I$(EUNITPATH)/inc 

SRC=bencode.erl tests.erl
BEAM=$(SRC:.erl=.beam)

all: $(BEAM)

%.beam : %.erl
	$(ERLC) $(ERLFLAGS) $<

test: $(BEAM)
	erl -noshell -pa $(EUNITPATH)/ebin -s tests -s init stop

clean:
	rm -f $(BEAM) *~
