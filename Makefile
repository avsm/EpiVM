DB = --user
PREFIX = $(HOME)
GHCOPTS = 

package:
	cabal build

cabal-package:
	cabal sdist

configure:
	cabal configure --user --ghc --prefix=$(PREFIX)
#	cd Epic; echo "module Epic.Prefix where libprefix=\"$(PREFIX)\"" > Prefix.hs

rts:
	$(MAKE) -C evm

install: .PHONY
	#$(MAKE) -C evm install PREFIX=$(PREFIX)
	#$(MAKE) -C lib install PREFIX=$(PREFIX)
	cabal install $(DB)

unregister:
	cabal unregister $(DB)

doc:
	cabal haddock

clean:
	cabal clean
	$(MAKE) -C evm clean
	cd compiler; rm -f *.o *.hi epic

test:
	make -C tests

epic: .PHONY configure package install
	cd compiler; ghc $(GHCOPTS) Main.lhs -o epic -package epic

epic_install: epic
	install compiler/epic $(PREFIX)/bin

.PHONY:
