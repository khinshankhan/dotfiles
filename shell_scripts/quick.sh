#! /usr/bin/env bash
set -euo pipefail

#instead of being run like:
#`sh quick.sh [repository]`
#this should be run as:
#`. quick.sh [repository]` OR `source quick.sh [repository]`

#note, the following information is for example purposes
#one should replace wherever a comment indicates to
#take for example you wish to clone https://github.com/kkhan01/shell_scripts.git


#set equal to whomever the repository's owner/creator is
owner="kkhan01"
#set equal to whatever the repository's name is
dir="shell_scripts"

# set the repository as the first argument if available
repository="${1:-${owner}/${dir}}"

git clone https://github.com/"${repository}".git
cd "$(basename "${repository}")"

