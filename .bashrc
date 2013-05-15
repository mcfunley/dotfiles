export CLICOLOR=1
export GREP_OPTIONS='--color=auto'
export LSCOLORS=Exfxcxdxbxegedabagacad
export PATH=/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH
export PS1='\n[$PWD]\n\u@\h $ '

alias emr=elastic-mapreduce

if [ -a ~/.bash_search ]; then
    source ~/.bash_search
fi

if [ -a ~/.bash_hadoop ]; then
    source ~/.bash_hadoop
fi

if [ -a ~/.bash_vm_public ]; then
    source ~/.bash_vm_public
fi

export ANT_OPTS="-Xms2048m -Xmx4096m"
export SBT_OPTS=-XX:MaxPermSize=2G

alias simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app'
