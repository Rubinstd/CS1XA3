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


check_uptodate
check_uncommited
create_todo_list
check_haskell_errors

