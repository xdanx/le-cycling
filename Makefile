all:
	ghc -O3 -threaded -with-rtsopts="-N" --make Main.hs -o Cycling
	find . -regextype sed -regex ".*/*\.\(o\|hi\)" | xargs rm
clean:
	find . -regextype sed -regex ".*/*\.\(o\|hi\)" | xargs rm
