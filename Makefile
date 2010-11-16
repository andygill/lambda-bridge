boot::
	cabal configure
	cabal build

bridges::
	mkdir -p bridges/bin

	make -C bridges/cat
	make -C bridges/cat install

