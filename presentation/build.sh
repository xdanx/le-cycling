#!/bin/sh
while [ `inotifywait -e modify 1.tex`]; do
	make all
done
