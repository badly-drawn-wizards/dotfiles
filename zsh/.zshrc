HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd

source "$HOME/.antigen/antigen.zsh"

antigen use oh-my-zsh

antigen bundle lein

antigen bundle git
antigen bundle rsync

antigen bundle pip
antigen bundle virtualenv

antigen bundle command-not-found

antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme candy

antigen apply

bindkey -v
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward
bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward

eval $(thefuck --alias)

compdef gpg2=gpg

# Emacs
export EDITOR=em
unset zle_bracketed_paste

# OPAM configuration
. /home/reuben/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# virtualenvwrapper
source /usr/bin/virtualenvwrapper.sh

# NVM
source /usr/share/nvm/init-nvm.sh

# Aliases

alias jap="env LC_ALL=\"ja_JP.UTF8\""
alias wine-steam="wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe"
alias t="tmux attach || tmux new"


# tabtab source for yo package
# uninstall by removing these lines or running `tabtab uninstall yo`
[[ -f /home/reuben/.nvm/versions/node/v8.4.0/lib/node_modules/yo/node_modules/tabtab/.completions/yo.zsh ]] && . /home/reuben/.nvm/versions/node/v8.4.0/lib/node_modules/yo/node_modules/tabtab/.completions/yo.zsh
