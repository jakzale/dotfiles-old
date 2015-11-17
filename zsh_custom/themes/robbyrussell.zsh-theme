local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ %s)"
PROMPT='${ret_status}%{$fg_bold[green]%}%p %{$fg[cyan]%}%c%{$fg_bold[blue]%}$(_prompt_info)%{$fg_bold[blue]%} % %{$reset_color%}'

_prompt_info() {
    my_sandbox_info=$(cabal_sandbox_info)
    my_git_info=$(git_prompt_info)

    if [ -n "$my_sandbox_info" ]; then
        echo -n "" $my_sandbox_info
    fi

    if [ -n "$my_git_info" ]; then
        echo -n "" $my_git_info
    fi
}

# PROMPT='${ret_status}%{$fg_bold[green]%}%p %{$fg[cyan]%}%c %{$fg_bold[blue]%}$(cabal_sandbox_info)%{$fg_bold[blue]%}$(git_prompt_info) % %{$reset_color%}'
ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
