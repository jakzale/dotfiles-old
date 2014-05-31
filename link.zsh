#!/usr/local/bin/zsh
#
# Linked to the zsh installed by Homebrew

# Iterating over all dotfiles and just linking them to HOME
for file (.*(ND-.))ln -s `pwd`/$file $HOME/$file

