include ../vsn.mk
VSN=$(GB_CONF_VSN)

EBIN = ../ebin

APP_NAME = gb_conf
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

MODULES = \
    gb_conf_app \
    gb_conf_sup \
    gb_conf

ERL_FILES = $(MODULES:%=%.erl)

INCLUDE_DIRS = \
	./

.PHONY: all clean

all: $(EBIN) $(APP_TARGET)
	erlc -I $(INCLUDE_DIRS) -o $(EBIN) $(ERL_FILES)

$(EBIN):
	mkdir ../ebin

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	rm ../ebin/*.app
	rm ../ebin/*.beam