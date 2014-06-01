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

#alias node="env NODE_NO_READLINE=1 rlwrap node"

#export PERL_LOCAL_LIB_ROOT="$HOME/perl5";
#export PERL_MB_OPT="--install_base $HOME/perl5";
#export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
#export PERL5LIB="$HOME/perl5/lib/perl5/darwin-thread-multi-2level:$HOME/perl5/lib/perl5";
#export PATH="/Users/jakub/perl5/bin:$PATH";

# Setting up Google go
#export GOPATH="$HOME/Developer/go-ext:$HOME/Developer/go"
#export GOROOT="/usr/local/Cellar/go/1.1.2"
#export PATH=$PATH:${GOPATH//://bin:}/bin

# Marked completion
#zstyle ':completion:*:*:mark' file-patterns \
    #'*.(md|mdown|markdown):markdown' '*:all-files'

# Guard start command
#alias grd='guard start -i 1>> log/implementation.log 2>> log/guard.log'
#alias grd='guard start 2>> log/guard.log'

# Add racket to command line
export PATH=$PATH:"/Applications/Racket v5.92/bin/"

# Amazon Web Services


export CLASSPATH=".:/usr/local/Cellar/antlr/4.2/antlr-4.2-complete.jar:$CLASSPATH"
alias grun="java org.antlr.v4.runtime.misc.TestRig"
