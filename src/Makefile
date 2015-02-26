include ../vsn.mk
VSN=$(GB_CONF_VSN)

EBIN = ../ebin

APP_NAME = gb_conf
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

MODULES = \
	gb_conf \
	gb_conf_db \
	gb_conf_env \

ERL_FILES = $(MODULES:%=%.erl)
ERL_BEAMS = $(MODULES:%=$(EBIN)/%.beam)

INCLUDE_DIRS = \
	../include

ERL_INCLUDE = $(INCLUDE_DIRS:%=-I %)

.PHONY: all clean beams

all: $(EBIN) $(APP_TARGET) beams

beams: $(ERL_BEAMS)

$(EBIN)/%.beam: %.erl
	erlc $(ERL_INCLUDE) -o $(EBIN) $<

$(EBIN):
	mkdir $@

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	rm ../ebin/*.app
	rm ../ebin/*.beam
