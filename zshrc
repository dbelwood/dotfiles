export ZSH=$HOME/.oh-my-zsh
root_dir=~/.dotfiles

# Oh-My-Zsh configuration
ZSH_THEME="sunaku"
plugins=(brew common-aliases git github jsontools urltools)
source $ZSH/oh-my-zsh.sh

source $root_dir/zsh_include/aliases
source $root_dir/zsh_include/aliases.local
source $root_dir/zsh_include/functions
source $root_dir/zsh_include/functions.local
source $root_dir/zsh_include/init
source $root_dir/zsh_include/init.local
