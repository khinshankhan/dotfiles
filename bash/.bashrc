#!/bin/bash

# Test for an interactive shell.
if [[ $- != *i* ]] ; then
    return
fi

# Use bash completion if available
[[ -f /usr/share/bash-completion/bash_completion ]] && source /usr/share/bash-completion/bash_completion
[[ -e /etc/bash/bashrc.d/bash_completion.sh ]] && source /etc/bash/bashrc.d/bash_completion.sh

for file in ~/.bash_{aliases,functions,exports,local}; do
    if [ -r "$file" ] && [ -f "$file" ]; then
        source "$file"
    fi
done

# Enable colors for ls, etc.
for file in {"${HOME}/.dir_colors","/etc/DIR_COLORS"}; do
    if [[ -f "$file" ]]; then
        eval "$(dircolors -b "$file")"
        break
    fi
done


shopt -s autocd                                 # Name of directory executed as if it was argument to `cd`
shopt -s cdspell                                # Check and correct slight spelling errors
shopt -s checkjobs                              # List status of jobs before exiting
shopt -s checkwinsize                           # Check the window size after every command
shopt -s cmdhist                                # Save all lines of multi-line command to same history entry
shopt -s dirspell                               # Check and correct slight spelling errors
shopt -s dotglob                                # Include files starting with `.` in pathname expansion
shopt -s extglob                                # Extended pattern matching features
shopt -s globstar                               # `**` matches all files and directories/subdirectories
shopt -s histappend                             # Append to history file instead of overwriting
shopt -s no_empty_cmd_completion                # Do not search for completions if line is empty

export HISTCONTROL="$HISTCONTROL erasedups:ignoreboth"
export HISTFILESIZE=
export HISTIGNORE="?:??:ls:[bf]g:exit:pwd:clear:mount:umount:history"
export HISTSIZE=

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export PATH=$PATH:${HOME}/.myscripts
