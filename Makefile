DB = --user
PREFIX = $(HOME)
GHCOPTS = 

package: rts
	runhaskell Setup.lhs build

configure:
	runhaskell Setup.lhs configure --user --ghc --prefix=$(PREFIX)
	cd Epic; echo "module Epic.Prefix where libprefix=\"$(PREFIX)\"" > Prefix.hs

rts:
	$(MAKE) -C evm

install: .PHONY rts
	$(MAKE) -C evm install PREFIX=$(PREFIX)
	#$(MAKE) -C lib install PREFIX=$(PREFIX)
	runhaskell Setup.lhs install $(DB)

unregister:
	runhaskell Setup.lhs unregister $(DB)

doc:
	runhaskell Setup.lhs haddock

clean:
	runhaskell Setup.lhs clean
	$(MAKE) -C evm clean
	cd compiler; rm -f *.o *.hi epic

test:
	make -C tests

epic: .PHONY configure package install
	cd compiler; ghc $(GHCOPTS) --make Main.lhs -o epic -package epic

epic_install: epic
	install compiler/epic $(PREFIX)/bin

.PHONY:
