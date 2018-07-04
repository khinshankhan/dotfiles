#Customizing the display
##move this to after the if statement for the PS for better ettiquette, thought you could technically get rid of the if
bold=$(tput bold)
normal=$(tput sgr0)
#colors and indication of command prompt
RESET="\[\017\]"
NORMAL="\[\033[0m\]"
grey='\[\033[1;30m\]'
red='\[\033[0;31m\]'
RED='\[\033[1;31m\]'
green='\[\033[0;32m\]'
GREEN='\[\033[1;32m\]'
yellow='\[\033[0;33m\]'
YELLOW='\[\033[1;33m\]'
purple='\[\033[0;35m\]'
PURPLE='\[\033[1;35m\]'
white='\[\033[0;37m\]'
WHITE='\[\033[1;37m\]'
blue='\[\033[0;34m\]'
BLUE='\[\033[1;34m\]'
cyan='\[\033[0;36m\]'
CYAN='\[\033[1;36m\]'
SMILEY="${WHITE}:)${NORMAL}"
FROWNY="${RED}:(${NORMAL}"
#determine status of last command
SELECT="if [ \$? = 0 ]; then echo \"${SMILEY}\"; else echo \"${FROWNY}\"; fi"
#various aspects of command prompt
top="⮳ " #"╭" #"╔═"
bottom="⮱ " #"╰" #"╚═"
top="${bold}$top${normal}"
bottom="${bold}$bottom${normal}"
cwrn="${green}[\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)] \w${normal}"
#all together now
PS1="$top[${yellow}\u@\h${normal}][\`${SELECT}\`]\n$bottom$cwrn\n  $ ${white}"

#Git
##clone a repo (http) by specifying repo and then username (or using defaults)
function http() {
    REPO=${1:-"scripts"}
    USER=${2:-"kkhan01"}
    git clone "https://github.com/$USER/$REPO.git";
}
##list out 100 repos associated with a github user (alphabetical into list.txt)
function repolist() {
    USER=${1:-"kkhan01"}
    curl "https://api.github.com/users/$USER/repos?per_page=100" | grep -o 'git@[^"]*' > list.txt;
    sed -i -e 's/git@/http:\/\//g' ./list.txt;
    #sed 's/\.git/ /g' ./list.txt; #removes the .git
}

#Emacs
##have spacemacs files in .spacemacs/.emacs.d
##git clone git@github.com:syl20bnr/spacemacs.git ~/.spacemacs/.emacs.d
function spacemacs() {
    HOME=~/spacemacs emacs "$@"
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
