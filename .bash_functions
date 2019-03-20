#!/bin/bash
# File: .bash_functions

#Git
##########
# Note:
# If you don't put double quotes around your variables, your function will break if they have a space in them (we want this behavior for these functions!)
##########
##clone a repo (http) by specifying repo and then username (or using defaults)
function ghttp() (
    REPO=${1:-"kkhan01.github.io"}
    USER=${2:-"kkhan01"}
    NAME=${3:-$REPO}
    git clone "https://github.com/$USER/$REPO.git" $NAME;
)

##clone a repo (http) by specifying repo and then username (or using defaults)
function gssh() (
    REPO=${1:-"kkhan01.github.io"}
    USER=${2:-"kkhan01"}
    NAME=${3:-$REPO}
    git clone "git@github.com:$USER/$REPO.git" $NAME;
)

##list out 100 repos associated with a github user (alphabetical into list.txt)
function repolist() {
    USER=${1:-"kkhan01"}
    curl "https://api.github.com/users/$USER/repos?per_page=100" | grep -o 'git@[^"]*' > list.txt;
    sed -i -e 's/git@/http:\/\//g' ./list.txt;
    #sed 's/\.git/ /g' ./list.txt; #removes the .git
}
##name the branch you're on quickly
function gbranch(){
    git branch
}

#Emacs
##have spacemacs files in .spacemacs/.emacs.d
##git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d/.emacs.d
function spacemacs() {
    HOME=~/.spacemacs.d emacs "$@"
}
##kind of niche, but Zamansky's tutorials from http://cestlaz.github.io/stories/emacs/ in .zemacs/.emacs.d
function zemacs() {
    emacs -Q -l ~/.zemacs/.emacs.d/init.el "$@"
}


function pdfc(){
    libreoffice --convert-to "pdf" "$@"
}

#NOT FULLY FUNCTION YET

##SSH with display
<< --MULTILINE-COMMENT--
function sssh() {
    URL=${1:-"khinshan.khan@ssh.com"} #fake url for repo
    x
    ssh -XY "$URL"
}
--MULTILINE-COMMENT--
