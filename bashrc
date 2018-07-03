#Customizing the display
##move this to after the if statement for the PS for better ettiquette, thought you could technically get rid of the if
bold=$(tput bold)
normal=$(tput sgr0)
top="⮳ " #"╭" #"╔═"
bottom="⮱ " #"╰" #"╚═"
PS1="${bold}$top${normal}\w\n${bold}$bottom${normal}$ "

#Display Redirection
export DISPLAY=localhost:0.0

#Daily Commands
alias work="cd ~/Documents/code/current"

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

#NOT FULLY FUNCTION YET

##SSH with display
<< --MULTILINE-COMMENT--
function sssh() {
    URL=${1:-"khinshan.khan@ssh.com"} #fake url for repo
    x
    ssh -XY "$URL"
}
--MULTILINE-COMMENT--
