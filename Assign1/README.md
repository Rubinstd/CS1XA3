Assignment 1 - Project Analyzer

Assignment Required Features:
1. Uses git status to compare the local repo to the remote repo.
2. Puts the output of git diff into changes.log in order to show any uncommited changes.
3. Greps for #TODO within any files (excluding the todo.log and ProjectAnalyze.sh) and places the results into todo.log.
4. Searches for haskell files, and checks them for any errors, placing the output into error.log.

Custom Features:
1. git log is a fantastic tool for searching through your commits, but you really don't want to see every commit since the dawn of time. So by calling the script with the two arguments
   sl "searchstring" (sl standing for search log), the script will search through your (hopefully) detailed commit messages and place the "useful" commits containing the searchstring
   into a file called search.log.
2. Sometimes you're working on these projects in a group. Now you're obviously a pro programmer who never messes things up, but unfortunately I can't say the same for your buddies.
   By using the two arguments sla "authorname" (sla standing for search log by author), the script will place only the commits whose authors are the given name into the search.log file
   that way you can check what those pesky peers of yours have done to your beautiful code. (Note: Both this and the first function can be done through git commands, I just wanted to add
   them in my own way so that I could actually edit which information gets found and how if I ever wanted to change the formatting myself)
3. This is a custom function embedded into my haskell error checking function. Instead of just blindly checking for haskell errors, I first check to see if the hs file has a main function. 
   If it does not, I add in a line at the end of the file with an undefined main. This allows the error checker to then continue to check the rest of the code for any errors. 
   I also made sure that the stdout (which is normally just a couple of lines that say that the file has compiled the main) aren't printed while the script runs or in the error.log as I 
   found them quite useless. Only the stderr (the actual error messages) are sent to error.log. Lastly I made sure that there is a header for each hs file indicating it's directory to show 
   that the error messages after said line are for the given files.
4. The script contains a function that works the same as the haskell error checking function but for python files.
5. Added in a built in search through all files for a search string feature when you give the script the inputs sds "searchstring" (sds standing for search directories for string). This is 
   simply a function I added because programmers are lazy and typing out a whole grep statement can be tedious.
6. Added a built in search for files with a search string in the name when you give the script the inputs sdf "filename". This search is not case sensitive and returns all files (and their
   directories). This function is useful because typing out the whole find statement can be tedious and programmers are lazy.
7. Added a custom feature that sets up a new directory when the script is given the extra parameters sdir "directoryname" (sdir standing for setup directory). This new directory has a 
   README.md file in it by default with the date it was created. 
