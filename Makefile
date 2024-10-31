# John, 2015-11-04 Makefile for lab3 testsuite
# Andreas, 2024-10-31 ported to own repo

.PHONY: suite
suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh plt-test-lab3.cabal plt-test-lab3.hs Makefile-test Runtime.java \
                        good/**/*.cc good/**/*.cc.*put dir-for-path-test/one-more-dir/simple.cc
	./build-tarball.sh

.PHONY: test
test: test-stack test-cabal

.PHONY: test-cabal
test-cabal:
	cabal build all

.PHONY: test-stack
test-stack:
	stack build

# EOF
