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
3. This is a custom function embedded into my haskell error checking function. Instead of just blindly checking for haskell errors, I first check to see if the hs file has a main function. 
   If it does not, I add in a line at the end of the file with an undefined main. This allows the error checker to then continue to check the rest of the code for any errors. 
   I also made sure that the stdout (which is normally just a couple of lines that say that the file has compiled the main) aren't printed while the script runs or in the error.log as I 
   found them quite useless. Only the stderr (the actual error messages) are sent to error.log. Lastly I made sure that there is a header for each hs file indicating it's directory to show 
   that the error messages after said line are for the given files.
