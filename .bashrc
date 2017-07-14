export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export LSCOLORS=Exfxcxdxbxegedabagacad
export PATH=/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH
export PS1='\n[$PWD]\n\u@\h $ '

export PYENV_VIRTUALENV_DISABLE_PROMPT=1
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

if [ -a ~/.bash_aws ]; then
    source ~/.bash_aws
fi

if [ -a ~/.bash_java ]; then
    source ~/.bash_java
fi

if [ -a ~/.bash_ruby ]; then
    source ~/.bash_ruby
fi

alias simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app'
alias coda='brew update && brew upgrade --all && brew cleanup && docker images | grep "<none>" | awk "{ print $3 }" | xargs docker rmi'

alias gist='gist -p'
alias ls='ls -G'
