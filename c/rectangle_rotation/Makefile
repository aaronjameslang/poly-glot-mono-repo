CC = gcc
CFLAGS = -Wall -Wextra -std=c99
CRITERION_BREW_PATH = $(shell brew --prefix criterion 2>/dev/null)
CRITERION_FLAGS = -I$(CRITERION_BREW_PATH)/include -L$(CRITERION_BREW_PATH)/lib -lcriterion

test: test.c solution.c
	$(CC) $(CFLAGS) -o test test.c solution.c $(CRITERION_FLAGS)
	./test
	rm -f test
