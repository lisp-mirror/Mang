PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin

all: build/mang
.PHONY: all

build/mang: mang.asd package.lisp sbclinit \
	utility/anaphora.lisp utility/buffered-stream.lisp \
	utility/constant-functions.lisp utility/convenience.lisp \
	utility/distribution.lisp utility/finite-state-transducer.lisp \
	utility/list.lisp utility/memoization.lisp utility/nfsm-dfsm.lisp \
	utility/parser-combinator.lisp utility/pointer.lisp utility/set.lisp \
	source/blocks.lisp source/categories.lisp source/features.lisp \
	source/gen.lisp source/globals.lisp source/glyphs.lisp \
	source/markov.lisp source/read-mang.lisp source/sonority.lisp \
	source/sound-change.lisp source/word.lisp
	sbcl --noinform --disable-ldb --lose-on-corruption \
		--userinit ./sbclinit \
		--load ./compile-mang.lisp
	mkdir -pv build
	mv -v ./mang build/mang

.PHONY: clean
clean:
	rm -rvf ./build

.PHONY: install
install: all
	mkdir -pv $(BINDIR)
	cp -v build/mang $(BINDIR)/mang
