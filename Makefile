all:
	ghc -O2 --make Main.hs -o Cycling
	rm *.o *.hi
