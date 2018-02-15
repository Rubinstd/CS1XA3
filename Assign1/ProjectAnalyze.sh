#!/bin/bash



git status>changes.log

grep -r "#TODO">todo.log . --exclude "todo.log" --exclude "ProjectAnalyze.sh"
