default:
	ghc --make -threaded -rtsopts Interp.lhs

run:
	./Interp +RTS -N2
