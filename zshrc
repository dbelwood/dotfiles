export ZSH=$HOME/.oh-my-zsh

# Load paths
source ./zsh_include/paths

# Oh-My-Zsh configuration
ZSH_THEME="bureau"
plugins=(chruby brew common-aliases git github jsontools taskwarrior urltools vagrant web-search)
source $ZSH/oh-my-zsh.sh

source ./zsh_include/exports
source ./zsh_include/aliases
source ./zsh_include/functions
source ./zsh_include/init
