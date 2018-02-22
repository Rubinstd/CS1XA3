Assignment 1 - Project Analyzer

Assignment Required Features:
1. Uses a git fetch and compares the local repo to the remote repo.
2. Puts the output of git diff into changes.log in order to show any uncommited changes.
3. Greps for #TODO within any files (excluding the todo.log and ProjectAnalyze.sh) and places the results into todo.log.
4. Searches for haskell files, and checks them for any errors, placing the output into error.log.

Custom Features:
1. git log is a fantastic tool for searching through your commits, but you really don't want to see every commit since the dawn of time. So by calling the script with the two arguments
   sl "searchstring" (sl standing for search log), the script will search through your (hopefully) detailed commit messages and place the "useful" commits containing the searchstring
   into a file called search.log.
2. Sometimes you're working on these projects in a group. Now you're obviously a pro programmer who never messes things up, but unfortunately I can't say the same for your buddies.
   By using the two arguments sla "authorname" (sla standing for search log by author), the script will place only the commits whose authors are the given name into the search.log file
   that way you can check what those pesky peers of yours have done to your beautiful code. 
