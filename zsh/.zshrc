HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd

stty -ixon

source "$HOME/.antigen/antigen.zsh"

antigen use oh-my-zsh

antigen bundle direnv

antigen bundle git
antigen bundle rsync

antigen bundle pip
antigen bundle virtualenv

antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle spwhitt/nix-zsh-completions

antigen theme candy

antigen apply

zstyle ':completion:*' list-colors

bindkey -v
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward
bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward

eval $(thefuck --alias)


# Aliases
alias jap="env LC_ALL=\"ja_JP.UTF8\""
alias wine-steam="wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe"
alias t="tmux attach || tmux new"
