.PHONY: all prepare lint compile test clean

all: prepare lint compile test

prepare:
	eldev prepare
lint:
	eldev lint
compile:
	eldev compile --warnings-as-errors
test:
	eldev test
clean:
	eldev clean
