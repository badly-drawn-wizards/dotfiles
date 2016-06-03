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

antigen bundle pip
antigen bundle virtualenv

antigen bundle command-not-found

antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme $HOME/.antigen/private/themes fino

antigen apply

eval $(thefuck --alias)

# Emacs prints the escape character
unset zle_bracketed_paste

# OPAM configuration
. /home/reuben/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# virtualenvwrapper
source /usr/bin/virtualenvwrapper.sh
