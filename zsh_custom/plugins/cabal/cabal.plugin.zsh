# Slightly improved support for cabal
# TODO
#   * currently only a combination of cabal package && cabal sandbox is supported
#       - should I support more (this seems suitable for my projects
#       - it is not sure how I could support more
#   * add abbreviation for project names, as some might get quite long
#   * improve handling of multiple status lines in the prompt

# Bear in mind that cabal does not know about a sandbox in the parent directory
# function cabal_sandbox_info() {
#     # Find cabal file
#     local cabal_file=$(_find_cabal_file) cabal_dir
#     local cabal_prefix=" λ:(" cabal_suffix="%{$fg[blue]%})%{$reset_color%}"
#     local cabal_name="" cabal_box

#     if [ -n "$cabal_file" ]; then
#         # Getting the name of the project
#         cabal_name=$(sed -n -e 's/^name:[   ]*\([^  ]*\)[   ]*/\1/p' $cabal_file)

#         if [ -z "$cabal_name" ]; then
#             cabal_name="ε"
#         elif [ $#cabal_name -gt 5 ]; then
#             cabal_name="$cabal_name[1,5]…"
#         fi

#         cabal_dir=$(dirname $cabal_file)
#         if [ -f "$cabal_dir/cabal.sandbox.config" ]; then
#             cabal_box="%{$fg[green]%}"
#         else
#             cabal_box="%{$fg[red]%}"
#         fi

#         echo "$cabal_prefix$cabal_box$cabal_name$cabal_suffix"
#     fi
# }

function cabal_sandbox_info() {
    cabal_prefix="λ:(" cabal_suffix="%{$fg_bold[blue]%})%{$reset_color%} "
    cabal_files=(*.cabal(N))
    if [ $#cabal_files -gt 0 ]; then
        if [ -f cabal.sandbox.config ]; then
            echo "$cabal_prefix%{$fg[green]%}✔︎%{$reset_color%}$cabal_suffix"
        else
            echo "$cabal_prefix%{$fg[red]%}✘%{$reset_color%}$cabal_suffix"
        fi
    fi
}

# Sandbox aware ghc, ghci and runhaskell
alias ghc="cabal exec ghc -- "
alias ghci="cabal exec ghci -- "
alias runhaskell="cabal exec runhaskell -- "

function _cabal_commands() {
    local ret=1 state
    _arguments ':subcommand:->subcommand' && ret=0

    case $state in
      subcommand)
        subcommands=(
          "bench:Run the benchmark, if any (configure with UserHooks)."
          "build:Compile all targets or specific targets."
          "check:Check the package for common mistakes."
          "clean:Clean up after a build."
          "configure:Prepare to build the package."
          "copy:Copy the files into the install locations."
          "exec:Run a command with the cabal environment"
          "fetch:Downloads packages for later installation."
          "freeze:Freeze dependencies."
          "get:Gets a package's source code."
          "haddock:Generate Haddock HTML documentation."
          "help:Help about commands."
          "hscolour:Generate HsColour colourised code, in HTML format."
          "info:Display detailed information about a particular package."
          "init:Interactively create a .cabal file."
          "install:Installs a list of packages."
          "list:List packages matching a search string."
          "register:Register this package with the compiler."
          "repl:Open an interpreter session for the given target."
          "report:Upload build reports to a remote server."
          "run:Runs the compiled executable."
          "sandbox:Create/modify/delete a sandbox."
          "sdist:Generate a source distribution file (.tar.gz)."
          "test:Run the test suite, if any (configure with UserHooks)."
          "update:Updates list of known packages."
          "upload:Uploads source packages to Hackage."
        )
        _describe -t subcommands 'cabal subcommands' subcommands && ret=0
    esac

    return ret
}

compdef _cabal_commands cabal
