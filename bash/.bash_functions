#!/bin/bash
# File: .bash_functions

# Git

## clone a repo (http) by specifying repo and then username (or using defaults)
function ghttp() (
    REPO=${1:-"kkhan01.github.io"}
    USER=${2:-"kkhan01"}
    NAME=${3:-$REPO}
    git clone "https://github.com/$USER/$REPO.git" $NAME;
)

## clone a repo (http) by specifying repo and then username (or using defaults)
function gssh() (
    REPO=${1:-"kkhan01.github.io"}
    USER=${2:-"kkhan01"}
    NAME=${3:-$REPO}
    git clone "git@github.com:$USER/$REPO.git" $NAME;
)

## list out 100 repos associated with a github user (alphabetical into list.txt)
function repolist() {
    USER=${1:-"kkhan01"}
    curl "https://api.github.com/users/$USER/repos?per_page=100" | grep -o 'git@[^"]*' > list.txt;
    sed -i -e 's/git@/http:\/\//g' ./list.txt;
    #sed 's/\.git/ /g' ./list.txt; #removes the .git
}

# Emacs

## have spacemacs files in .spacemacs/.emacs.d
## git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d/.emacs.d
function spacemacs() {
    HOME=~/.spacemacs.d emacs "$@"
}

# Misc

## convert any file (eg a .cc) to a pdf
function pdfc(){
    libreoffice --convert-to "pdf" "$@"
}

## kind of niche, but use files in .zemacs/.emacs.d as emacs
function zemacs(){
    HOME=~/.zemacs.d emacs "$@"
}

## SSH with display
<< --MULTILINE-COMMENT--
function sssh() {
    URL=${1:-"fname.lname@ssh.com"} #fake url for repo
    x
    ssh -XY "$URL"
}
--MULTILINE-COMMENT--
