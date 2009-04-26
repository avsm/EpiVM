DB = --user
PREFIX = $(HOME)
GHCOPTS = 

package:
	runhaskell Setup.hs build

cabal-package:
	runhaskell Setup.hs sdist

configure:
	runhaskell Setup.hs configure --user --ghc --prefix=$(PREFIX)
#	cd Epic; echo "module Epic.Prefix where libprefix=\"$(PREFIX)\"" > Prefix.hs

rts:
	$(MAKE) -C evm

install: .PHONY
	#$(MAKE) -C evm install PREFIX=$(PREFIX)
	#$(MAKE) -C lib install PREFIX=$(PREFIX)
	runhaskell Setup.hs install $(DB)

unregister:
	runhaskell Setup.hs unregister $(DB)

doc:
	runhaskell Setup.hs haddock

clean:
	runhaskell Setup.hs clean
	$(MAKE) -C evm clean
	cd compiler; rm -f *.o *.hi epic

test:
	make -C tests

epic: .PHONY configure package install
	cd compiler; ghc $(GHCOPTS) Main.lhs -o epic -package epic

epic_install: epic
	install compiler/epic $(PREFIX)/bin

.PHONY:
