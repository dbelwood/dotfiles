export ZSH=$HOME/.oh-my-zsh
root_dir=~/.dotfiles

# Load paths
source $root_dir/zsh_include/paths

# Oh-My-Zsh configuration
ZSH_THEME="bureau"
plugins=(chruby brew common-aliases git github jsontools taskwarrior urltools vagrant web-search)
source $ZSH/oh-my-zsh.sh

source $root_dir/zsh_include/exports
source $root_dir/zsh_include/aliases
source $root_dir/zsh_include/functions
source $root_dir/zsh_include/init
