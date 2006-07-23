DB = --user
PREFIX = $(HOME)
GHCOPTS = 

package: rts
	runhaskell Setup.lhs build

configure:
	runhaskell Setup.lhs configure --user --ghc --prefix=$(PREFIX)
	cd EMachine; echo "module EMachine.Prefix where libprefix=\"$(PREFIX)\"" > Prefix.hs

rts:
	$(MAKE) -C evm

install: .PHONY rts
	$(MAKE) -C evm install PREFIX=$(PREFIX)
	$(MAKE) -C lib install PREFIX=$(PREFIX)
	runhaskell Setup.lhs install $(DB)

unregister:
	runhaskell Setup.lhs unregister $(DB)

doc:
	runhaskell Setup.lhs haddock

clean:
	runhaskell Setup.lhs clean
	$(MAKE) -C evm clean
	cd ESC; rm -f *.o *.hi esc

test:
	make -C tests

esc: .PHONY package install
	cd ESC; ghc $(GHCOPTS) --make Main.lhs -o esc -package epivm

esc_install: esc
	install ESC/esc $(PREFIX)/bin

.PHONY:
