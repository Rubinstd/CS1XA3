#!/bin/bash



git diff>changes.log

grep -r "#TODO">todo.log . --exclude "todo.log" --exclude "ProjectAnalyze.sh"

find -name "*.hs" -exec ghc -fno-code {} \;>error.log 
