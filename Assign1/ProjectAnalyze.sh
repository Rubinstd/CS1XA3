#!/bin/bash

#Uses git status to check if the local repo is up to date with the remote repo.
check_uptodate(){
git status > tmpstatus.txt #Don't forget to take out the writing thing
}

#Checks for all uncommited changes and puts them into changes.log. 
#This function overwrites any previous info in changes.log to reduce clutter.
check_uncommited(){
git diff HEAD --exclude "changes.log" >changes.log
}

#Greps the entire git repository (assuming the script is at the root) and puts all lines with #TODO into a todo.log.
#This function overwrites any previous info in todo.log to reduce clutter.
create_todo_list(){
grep -r "#TODO" . --exclude={todo.log,ProjectAnalyze.sh,changes.log}>todo.log
}

#Checks all .hs files for syntax errors. Additionally, it will check if there is a main in all haskell files and add an undefined main if needed.
check_haskell_errors(){

echo "" > error.log

find . -name "*.hs" -type f| while read line; do
	echo "Errors for \"$line\" :" >> error.log
	maincount=$(grep "main" "$line" | wc -l)
	if [ $maincount -eq 0 ]
	then
		echo "main = undefined">>$line
	fi
	ghc -fno-code "$line" 1>/dev/null 2>>error.log
	echo "" >>error.log
		
done

}

#Checks alls .py files for python syntax errors.
check_python_errors(){
echo "" >error_python.log
find . -name "*.py" -type f | while read line; do
	echo "Errors for \"$line\" :" >>error_python.log
	echo "" >>error_python.log
	python -m py_compile "$line"&>>error_python.log
	echo "" >>error_python.log
done
}

#Searches through git log for a given search string. Requires the search string as an input.
search_log(){

git log | grep "$1" -B 4 -A 1 > search.log

}

#Searches through git log for a given Authors commits. Requires the Author's name as an input.
search_log_author(){

git log | grep "Author: $1" -B 2 -A 3 > search.log

}

#Searches recursively through all files in all directories (assuming the script is in the root directory) for a given string.
search_dir_string(){
grep -r "$1" 
}

#Searches through all files in the directory (assuming the script is in the root directory) for files with a given search string in the name (this is not case sensitive)
search_dir_file(){
find -iname "*$1*"
}

#Runs all the standard check functions that you would naturally want to run if you were using the script just to analyze various aspects of your git repo.
if [ $# -eq 0 ]
then
	check_uptodate
	check_uncommited
	create_todo_list
	check_haskell_errors
	check_python_errors


#Runs any indicated extra parameter functions based on the inputs provided.
elif [ $# -eq 2  ]
then
	if [ "$1" = "sl" ]
	then
		search_log $2
	
	elif [ "$1" = "sla" ]
	then
		search_log_author $2
	elif [ "$1" = "sds" ]
	then
		search_dir_string $2
	elif [ "$1" = sdf ]
	then
		search_dir_file $2
	else
		echo "Invalid inputs"
	fi
else
	echo "Invalid number of inputs"
fi
