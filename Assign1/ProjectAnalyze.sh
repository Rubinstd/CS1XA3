#!/bin/bash

git fetch
git diff master origin/master

git diff>changes.log

grep -r "#TODO">todo.log . --exclude "todo.log" --exclude "ProjectAnalyze.sh"

find -name "*.hs" -exec ghc -fno-code {} \;>error.log 
