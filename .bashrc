export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export LSCOLORS=Exfxcxdxbxegedabagacad
export PATH=/opt/chef/bin:/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH
export PS1='\n[$PWD]\n\u@\h $ '

if [ -a ~/.bash_aws ]; then
    source ~/.bash_aws
fi

export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Home/

export ANT_OPTS="-Xms2048m -Xmx4096m"
export SBT_OPTS=-XX:MaxPermSize=2G

. virtualenvwrapper.sh
export WORKSPACE=.

if [ -a ~/.bash_java ]; then
    source ~/.bash_java
fi

alias simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app'
alias coda='brew update && brew upgrade --all && brew cleanup'

alias gist='gist -p'

export ANDROID_HOME=/usr/local/opt/android-sdk
