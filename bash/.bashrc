#Customizing the display
##move this to after the if statement for the PS for better ettiquette, thought you could technically get rid of the if
##########
# Note:
# You used `bold=$(tput bold)` here to get bold text, but you could also do `bold=\[\033[1m\]]`
# You don't have to, but could if you want to be consistent
# In the same manner, all the weird stuff below for the colors can be replaced
# with the proper $(tput <blahblah>) command. I personally don't though but Jeffrey does.
# Something about $(tput <blahblah>) being more portable or something.
##########
# Note:
# You forgot the to the "\[" and "\]" surrounding bold and normal.
# The purpose of these escaped brackets is to tell bash that the characters
# between them don't have any width. This lets bash calculate when to stop
# letting you backspace so you don't delete your prompt when holding backspace
##########
bold="\[$(tput bold)\]"
normal="\[$(tput sgr0)\]"
#colors and indication of command prompt
##########
# Note:
# You're using ansi escape sequences here to get colors here. They're used by
# using an escape sequence followed by something like `[1;30m`.
# \033 is the octal representation of the escape sequence, but it can also be 
# represented by \e which is much cleaner
##########
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
##########
# Note:
# If you don't put double quotes around your variables, your function will break if REPO or USER have a space in them
##########
##clone a repo (http) by specifying repo and then username (or using defaults)
function http() {
    REPO="${1:-"scripts"}"
    USER="${2:-"kkhan01"}"
    git clone "https://github.com/$USER/$REPO.git";
}
##list out 100 repos associated with a github user (alphabetical into list.txt)
function repolist() {
    USER="${1:-"kkhan01"}"
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
