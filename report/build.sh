#!/bin/sh
while inotifywait -e modify report.tex; do
	make all
done