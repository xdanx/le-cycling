#!/bin/sh
while [ 1 -eq 1 ]; do
	inotifywait -e modify 1.tex
	make all
done
