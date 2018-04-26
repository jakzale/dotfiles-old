# Some minor configurations
# Set-Alias -Name "ls"  -Value "ls -G" -Description "Colorized ls"
function Get-ChildItemColor {
    $args = ,"-G" + $args
    &/bin/ls $args
}

Set-Alias -Name "ls" Get-ChildItemColor

function wrap ([int]$colorCode, [string]$text)
{
    "`e[$($colorCode)m$($text)`e[0m"
}

function red ([string]$text)
{
    wrap 31 $text
}

function prompt
{
    # This will probably not work on windows
    # Are we in a git repo?
    function is_git
    {
        [bool](git rev-parse --git-dir 2>/dev/null)
    }

    # Is the repo empty?
    function is_repo_not_empty
    {
        [bool](git rev-list -n 1 --all 2>/dev/null)
    }

    # Get the name of the branch
    function git_branch
    {
        [string](git rev-parse --abbrev-ref HEAD)
    }

    # Is the git repo clean?
    function is_git_dirty
    {
        -not [bool](git diff-index --quiet HEAD --)
    } 




    
    # Making powershell prompt resembly the robbyrussel theme from oh-my-zsh
    # Getting the full path
    $fullPath = $ExecutionContext.SessionState.Path.CurrentLocation
    # Converting the fullPath to URI
    $pathUri = [uri]("file://" + $fullPath.Path)
    # Getting the directory
    $dirName = $pathUri.Segments[-1]

    # Getting the nestedPromptLevel
    $nestingIndicator = if ($nestedPromptLevel -gt 0)
        {
            "$('>' * ($nestedPromptLevel) ) "
        }
        else {
            ""
        }

    $gitIndicator = ""
    $gitStatusIndicator = ""
    # Checking the git indicator
    if (is_git) {
        $gitBranchName = "master"
        if (is_repo_not_empty) {
            $gitBranchName = "$(git_branch)"

            if (is_git_dirty) {
                $gitStatusIndicator = "✗ "
            }
        }

        $gitIndicator = "git:($(red $gitBranchName)) "
    }

    "➜  $($dirName) $($nestingIndicator)$($gitIndicator)$($gitStatusIndicator)`n➜  "
}

function prompt {
    "PS> "
}