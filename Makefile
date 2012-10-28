all:
	ghc -XTupleSections -O2 --make Main.hs -o Cycling
	rm *.o *.hi
