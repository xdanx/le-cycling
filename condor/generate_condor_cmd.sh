#!/bin/bash
FILE=$1
TIMES=$2
DIRECTORY=`pwd`
OUTPUT="condor_run_$FILE.cmd"

sed "s/\${filepath}/$FILE/" <condor_template.cmd | sed "s/\${times}/$TIMES/" | sed "s@\${cur_dir}@$DIRECTORY@" > $OUTPUT