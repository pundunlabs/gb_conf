include ./vsn.mk
VSN=$(BG_CONF_VSN)
APP_NAME = gb_conf

SUBDIRS = src

.PHONY: all subdirs $(SUBDIRS) edoc eunit clean

all: edoc subdirs eunit

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

edoc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}, {source_path, ["src", "test"]}]'

eunit:
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

clean:
	rm -f ./ebin/*

realclean: clean

