######################################################################
# Makefile
######################################################################

CC_DRIV  = -fpic -shared
CC_LIBS += -L /usr/local/lib -L /usr/local/lib/erlang25/lib -lgumbo
CC_INCS += -I /usr/local/include -I/usr/local/lib/erlang25/usr/include/

all: gumbo_driver.so gumbo_test

gumbo_driver.so:
	cc -o $@ $(CC_LIBS) $(CC_INCS) $(CC_DRIV) gumbo.c gumbo_driver.c

gumbo_test:
	cc -o $@ $(CC_LIBS) $(CC_INCS) gumbo.c gumbo_test.c

clean:
	rm gumbo_driver.so
	rm gumbo_test
