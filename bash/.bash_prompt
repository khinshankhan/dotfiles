# get default git prompt script
for file in {"/usr/lib/git-core/git-sh-prompt","/usr/share/git/git-prompt.sh","${HOME}/git-sh-prompt.sh"}; do
#for file in {"~/git_prompt"}; do
    if [ -e "$file" ]; then
        source "$file"
        break
    fi
done

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"

## customized command prompt display
PROMPT_COMMAND=_prompt
_prompt() {
    local EXIT=$?
    local RED="\[$(tput setaf 1)\]"
    local GREEN="\[$(tput setaf 2)\]"
    local YELLOW="\[$(tput setaf 3)\]"
    local BLUE="\[$(tput setaf 4)\]"
    local MAGENTA="\[$(tput setaf 5)\]"
    local CYAN="\[$(tput setaf 6)\]"
    local VIOLET="\[$(tput setaf 61)\]"
    local WHITE="\[$(tput setaf 7)\]"
    local GRAY="\[$(tput setaf 8)\]"
    local RESET="\[$(tput sgr0)\]"
    local BOLD="\[$(tput bold)\]"
    PS1=""

    # Show username and host
    #PS1+="[${CYAN}\u@\h${RESET}] "

    # Current working directory
    PS1+="${MAGENTA}\w${RESET}"

    # Show virtualenv info if we are in one
    if [[ -n "$VIRTUAL_ENV" ]]; then
        local virt=$(basename "$VIRTUAL_ENV")
        PS1+=" ${BOLD}${YELLOW}($virt)${RESET}"
    fi

    # show git branch and statuses if any
    PS1+=" ${BOLD}${VIOLET}$(__git_ps1 '(%s) ')"

    # new line now
    PS1+="${RESET}\n"
    # Exit status and prompt
    if [[ $EXIT != 0 ]]; then
        PS1+="${RED}"
    else
        PS1+=""
    fi
    PS1+="${BOLD}λ${RESET} "
}
