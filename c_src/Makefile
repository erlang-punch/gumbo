######################################################################
# GNU Makefile
# TODO: adapt the makefile to support different system
# TODO: 
######################################################################
CC_DRIV = -fpic -shared
CC_LIBS = -L /usr/lib
CC_LIBS += -lgumbo
CC_INCS = -I /usr/include

######################################################################
# Operating System Specific
######################################################################
UNAME := $(shell uname -s)
ifeq ($(UNAME),OpenBSD)
# required for gumbo (ports)
	CC_LIBS += -L /usr/local/lib
	CC_INCS += -I /usr/local/include

# required for erlang (ports)
	CC_LIBS += -L /usr/local/lib/erlang25/lib
	CC_INCS += -I /usr/local/lib/erlang25/usr/include/
endif

######################################################################
# ASDF
######################################################################
ifdef $(ASDF)
	CC_LIBS += -L ${HOME}/.asdf/installs/erlang/25.1.2/usr/lib
	CC_INCS += -I ${HOME}/.asdf/installs/erlang/25.1.2/usr/include
endif

######################################################################
# source
######################################################################
GUMBO_VERSION = 0.10.1
GUMBO_REPOSITORY = https://github.com/google/gumbo-parser/archive/refs/tags/v$(GUMBO_VERSION).tar.gz

all: test driver

driver: gumbo_driver.so

test: gumbo_test
	./gumbo_test

AUTOCONF_VERSION ?= 2.69
AUTOMAKE_VERSION ?= 1.16
source:
	ftp $(GUMBO_REPOSITORY)
	tar zxvf v$(GUMBO_VERSION).tar.gz
	cd gumbo-parser-$(GUMBO_VERSION) \
		&& ./autogen.sh \
		&& ./configure \
		&& gmake

clean:
	rm gumbo_driver.so
	rm gumbo_test

gumbo_driver.so:
	cc gumbo_erlang.c gumbo_driver.c $(CC_LIBS) $(CC_INCS) $(CC_DRIV) -o $@

gumbo_test:
	cc gumbo_erlang.c gumbo_test.c $(CC_LIBS) $(CC_INCS) -o $@
