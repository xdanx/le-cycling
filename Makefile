all:
	ghc -O2 --make Main.hs -o Cycling
	find . -regextype sed -regex ".*/*.\(o\|hi\)" | xargs rm
clean:
	find . -regextype sed -regex ".*/*.\(o\|hi\)" | xargs rm
