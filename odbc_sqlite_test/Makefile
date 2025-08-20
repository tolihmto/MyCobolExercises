# Makefile pour odbc_sqlite_test

COBJS = odbc_wrapper.o odbc_allocenv_setodbc3.o odbc_sqlconnect.o odbc_sqlgetdiagrec.o odbc_sqlexecdirect.o
CSRCS = odbc_wrapper.c odbc_allocenv_setodbc3.c odbc_sqlconnect.c odbc_sqlgetdiagrec.c odbc_sqlexecdirect.c

PROG = odbc_sqlite_demo
SRC  = odbc_sqlite_demo.cbl

ODBC_LIBS = -lodbc
COBFLAGS = -free
CCFLAGS = -fPIC -Wall -O2

all: $(PROG)

$(PROG): $(SRC) $(COBJS)
	cobc $(COBFLAGS) -x -o $@ $^ $(ODBC_LIBS)

%.o: %.c
	gcc $(CCFLAGS) -c $< -o $@

clean:
	rm -f $(PROG) $(COBJS)

.PHONY: all clean
