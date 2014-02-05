export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export LSCOLORS=Exfxcxdxbxegedabagacad
export PATH=/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH
export PS1='\n[$PWD]\n\u@\h $ '

if [ -a ~/.bash_aws ]; then
    source ~/.bash_aws
fi

export JAVA_HOME="$(/usr/libexec/java_home)"

export ANT_OPTS="-Xms2048m -Xmx4096m"
export SBT_OPTS=-XX:MaxPermSize=2G

. virtualenvwrapper.sh
export DJANGO_SETTINGS_MODULE=settings.dev
export SECRETS=$HOME/radico/keys/secrets
if [ -a $SECRETS ]; then
    source $SECRETS
fi

alias simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app'

alias gist='gist -p'

