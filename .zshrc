# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

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
plugins=(git brew osx taskwarrior cabal)

source "$ZSH/oh-my-zsh.sh"

# Setting up globals

## Setting up the editor variable
export EDITOR="vim"

## Setting up the OpenGL profiler
export GL_ENABLE_DEBUG_ATTACH=YES

## Set up node path
export NODE_PATH="/usr/local/lib/node_modules/"

# Helper functions
## Hack to silently load script
function source_silent() {
    [[ -s $1 ]] && source $1
}

## Helper function for handling PATH
function pathify() {
    export PATH=$1
}

# Setting up PATH
## Base PATH, local first
pathify "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin"

## Use Haskell, added first to use updated cabal
pathify "$HOME/Library/Haskell/bin:$PATH"

## Use Racket command line
pathify "$PATH:/Applications/Racket v6.1/bin/"


# Loading Scripts
## Load virtualenvwrapper
source_silent "/usr/local/bin/virtualenvwrapper.sh"

## Load RVM
source_silent "$HOME/.rvm/scripts/rvm"

## Load Z
source_silent "`brew --prefix`/etc/profile.d/z.sh"

## Load OPAM
source_silent "/Users/jakub/.opam/opam-init/init.zsh"


