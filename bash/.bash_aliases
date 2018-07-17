#Customizing the display

## Note:
## We used `bold=$(tput bold)` here to get bold text, but we could also do `bold=\[\033[1m\]]`
## You don't have to, but could if you want to be consistent
## In the same manner, all the weird stuff below for the colors can be replaced with the proper $(tput <blahblah>) command. I personally would recommend this, but the follow may be a bit easier to understand; however $(tput <dubdubdub>) is supposedly more portable or something.
bold="\[$(tput bold)\]"
normal="\[$(tput sgr0)\]"

# Colors!
RESET="\[\017\]"
NORMAL="\[\e[0m\]"
grey='\[\e[1;30m\]'
red='\[\e[0;31m\]'
RED='\[\e[1;31m\]'
green='\[\e[0;32m\]'
GREEN='\[\e[1;32m\]'
yellow='\[\e[0;33m\]'
YELLOW='\[\e[1;33m\]'
purple='\[\e[0;35m\]'
PURPLE='\[\e[1;35m\]'
white='\[\e[0;37m\]'
WHITE='\[\e[1;37m\]'
blue='\[\e[0;34m\]'
BLUE='\[\e[1;34m\]'
cyan='\[\e[0;36m\]'
CYAN='\[\e[1;36m\]'

## customized command prompt display
function prompt(){
    local SMILEY="${WHITE}:)${NORMAL}"
    local FROWNY="${RED}:(${NORMAL}"
    #determine status of last command
    local SELECT="if [ \$? = 0 ]; then echo \"${SMILEY}\"; else echo \"${FROWNY}\"; fi"
    local status="[\`${SELECT}\`]"
    #various aspects of command prompt
    local top="⮳ " #"╭" #"╔═"
    local bottom="⮱ " #"╰" #"╚═"
    local top="${bold}$top${normal}"
    local bottom="${bold}$bottom${normal}"
    local cwrn="${green} \w${normal}"
    #get the date and time.
    #See `man bash` in section 'PROMPTING' for a list of all backslash-escaped special characters
    #For the specifcs of what the % characters are, look at `man 3 strftime`
    local date="\D{%m/%d}"
    local time="\D{%I:%M}"
    local rn="[${yellow}$date${white}|${yellow}$time${normal}]"
    local ourinfo="[${yellow}\u@\h${normal}]"
    #all together now
    export PS1="$top$ourinfo $rn $status\n$bottom$cwrn $ ${white}"
}
prompt

#Git
##########
# Note:
# If you don't put double quotes around your variables, your function will break if they have a space in them (we want this behavior for these functions!)
##########
##clone a repo (http) by specifying repo and then username (or using defaults)
function http() {
    REPO=${1:-"scripts"}
    USER=${2:-"kkhan01"}
    NAME=${3:-$REPO}
    git clone "https://github.com/$USER/$REPO.git" $NAME;
}
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
##git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d/.emacs.d
function spacemacs() {
    HOME=~/.spacemacs.d emacs "$@"
}
##kind of niche, but Zamansky's tutorials from http://cestlaz.github.io/stories/emacs/ in .zemacs/.emacs.d
function zemacs() {
    emacs -Q -l ~/.zemacs/.emacs.d/init.el "$@"
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
