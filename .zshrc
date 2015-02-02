export ZSH=$HOME/.zsh
export OH_MY_ZSH=$HOME/.oh-my-zsh

# Load paths
source $ZSH/paths

# Oh-My-Zsh configuration
ZSH_THEME="bureau"
plugins=(chruby brew common-aliases git github jsontools taskwarrior urltools vagrant web-search)
source $OH_MY_ZSH/oh-my-zsh.sh

source $ZSH/exports
source $ZSH/aliases
source $ZSH/functions
source $ZSH/init
