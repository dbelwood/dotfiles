export ZSH=$HOME/.oh-my-zsh
root_dir=~/.dotfiles

# Load paths
source $root_dir/zsh_include/paths
source $root_dir/zsh_include/paths.local

# Oh-My-Zsh configuration
ZSH_THEME="sunaku"
plugins=(chruby brew common-aliases git github jsontools taskwarrior urltools vagrant web-search)
source $ZSH/oh-my-zsh.sh

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby ruby-2.2.1

source $root_dir/zsh_include/exports
source $root_dir/zsh_include/exports.local
source $root_dir/zsh_include/aliases
source $root_dir/zsh_include/aliases.local
source $root_dir/zsh_include/functions
source $root_dir/zsh_include/functions.local
source $root_dir/zsh_include/init
source $root_dir/zsh_include/init.local
