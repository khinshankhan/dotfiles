#instead of being run like:
#sh quick.sh
#this should be run as:
#. quick.sh

#note, the following information is for example purposes
#one should replace wherever a comment indicates to
#take for example you wish to clone https://github.com/kkhan01/shell_scripts.git


#set equal to whomever the repository's owner/creator is
owner="kkhan01"
#set equal to whatever the repository's name is
dir="shell_scripts"

git clone https://github.com/"$owner"/"$dir".git
cd ./"$dir"
