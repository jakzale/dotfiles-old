ZSH=$HOME/.oh-my-zsh

# ZSH_CUSTOMIZATION
ZSH_CUSTOM=$HOME/.zsh_custom

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
plugins=(git brew osx taskwarrior cabal rust)

source "$ZSH/oh-my-zsh.sh"

# Setting up globals

## Setting up the editor variable
export EDITOR="nvim"

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
# pathify "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin"

## Load BOXEN
source_silent "/opt/boxen/env.sh"

## Use Racket command line
pathify "$PATH:/Applications/Racket v6.1/bin"


# Add GHC 7.10.1 to the PATH, via http://ghcformacosx.github.io/
# export GHC_DOT_APP="/Applications/ghc-7.10.1.app"
# if [ -d "$GHC_DOT_APP" ]; then
#     pathify "${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
# fi

## Use Haskell, added first to use updated cabal
pathify "$HOME/Library/Haskell/bin:$PATH"

# Loading Scripts
## Load virtualenvwrapper
source_silent "`brew --prefix`/bin/virtualenvwrapper.sh"

## Load Z
source_silent "`brew --prefix`/etc/profile.d/z.sh"

## Set up NaCl_SDL to Pepper Canary
export NACL_SDK_ROOT="${HOME}/src/nacl_sdk/pepper_canary"
export CHROME_PATH="/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"

## Set up the nacl source root
export NACL_ROOT="${HOME}/src/native_client"

## Set up wrappers for gsoc
pathify "$HOME/src/haskell/PNaCl/gsoc/wrappers:$PATH"

# Set up depot_tools for gsoc
pathify "${HOME}/src/depot_tools:$PATH"

# Set up arcanist for gsoc
pathify "${HOME}/src/arcanist/bin:$PATH"

# Setting up JAVA_HOME correctly
export JAVA_HOME="$(/usr/libexec/java_home -v 1.7 2>/dev/null)"
