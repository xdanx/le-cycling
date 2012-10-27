all:
	ghc -XTupleSections --make Main.hs -o Cycling
	rm *.o *.hi
