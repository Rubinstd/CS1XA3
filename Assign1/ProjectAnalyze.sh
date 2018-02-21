#!/bin/bash

check_uptodate(){
git fetch
git diff master origin/master
}

check_uncommited(){
git diff>changes.log
}

create_todo_list(){
grep -r "#TODO">todo.log . --exclude "todo.log" --exclude "ProjectAnalyze.sh"
}

check_haskell_errors(){
find -name "*.hs" -exec ghc -fno-code {} \;&>error.log
}

search_log(){

git log | grep "$1" -B 4 -A 1 > search.log

}

check_uptodate
check_uncommited
create_todo_list
check_haskell_errors

if [ $# -eq 2  ]
then
	if [ "$1" = "sl" ]
	then
		search_log $2
	fi
fi
