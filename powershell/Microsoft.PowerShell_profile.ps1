# Some minor configurations
# Set-Alias -Name "ls"  -Value "ls -G" -Description "Colorized ls"

function prompt
{
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

    "➜  $($dirName) $($nestingIndicator)";
}