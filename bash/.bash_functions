#!/bin/bash
# File: .bash_functions

# Git
## list out 100 repos associated with a github user (alphabetical into list.txt)
function repolist() (
    USER=${1:-"kkhan01"}
    curl "https://api.github.com/users/$USER/repos?per_page=100" | grep -o 'git@[^"]*' > list.txt;
    sed -i -e 's/git@/http:\/\//g' ./list.txt;
    #sed 's/\.git/ /g' ./list.txt; #removes the .git
)
