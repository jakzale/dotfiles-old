# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# TODO: Clean this up a bit
#plugins=(git brew osx rails3 ruby bundler gem taskwarrior autojump hg)
#plugins=(git brew osx autojump)
# Removing autojum - trying z for now.
plugins=(git brew osx)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:$HOME/.cabal/bin
export PATH=/usr/local/bin:/usr/local/sbin:$PATH

# Adding latex support
export PATH=$PATH:/usr/texbin

# Hack to silently load script
function source_silent() {
    [[ -s $1 ]] && source $1
    # TODO: Add logging logic here
}

# Loading virtualenvwrapper
source_silent /usr/local/bin/virtualenvwrapper.sh

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
source_silent $HOME/.rvm/scripts/rvm

# Setting up the editor variable
export EDITOR=vim

# Setting up the OpenGL profiler
export GL_ENABLE_DEBUG_ATTACH=YES

# Loading the z command
source_silent `brew --prefix`/etc/profile.d/z.sh
alias j=z

# Add racket to command line
export PATH=$PATH:"/Applications/Racket v5.92/bin/"
