######################################################################
# Makefile
# TODO: adapt the makefile to support different system
# TODO: 
######################################################################

CC_DRIV  = -fpic -shared
CC_LIBS += -L /usr/lib
# CC_LIBS += -L /usr/local/lib
# CC_LIBS += -L /usr/local/lib/erlang25/lib
CC_LIBS += -lgumbo
CC_LIBS += -L ${HOME}/.asdf/installs/erlang/25.1.2/usr/lib

CC_INCS += -I /usr/include
# CC_INCS += -I /usr/local/include
# CC_INCS += -I /usr/local/lib/erlang25/usr/include/
CC_INCS += -I ${HOME}/.asdf/installs/erlang/25.1.2/usr/include

all: gumbo_driver.so gumbo_test test

test: gumbo_test
	./gumbo_test

clean:
	rm gumbo_driver.so
	rm gumbo_test

gumbo_driver.so:
	cc gumbo_erlang.c gumbo_driver.c $(CC_LIBS) $(CC_INCS) $(CC_DRIV) -o $@

gumbo_test:
	cc gumbo_erlang.c gumbo_test.c $(CC_LIBS) $(CC_INCS) -o $@


