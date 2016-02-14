HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd

bindkey -e
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward

source "$HOME/.antigen/antigen.zsh"

antigen use oh-my-zsh

antigen bundle git
antigen bundle rsync
antigen bundle command-not-found

antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme candy

antigen apply

eval $(thefuck --alias)
