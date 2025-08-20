# Makefile pour projet COBOL (compilation directe)

COBC = cobc
EXEC = mon_programme
SRCS = call_fibonacci.cbl ../fibonacci.cbl

all: $(EXEC)

$(EXEC): $(SRCS)
	$(COBC) -x -o $(EXEC) $(SRCS)

clean:
	rm -f $(EXEC)

.PHONY: all clean
