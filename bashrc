#! /usr/bin/env bash

export BASH_IT="/home/danb/.bash_it"
root_dir=~/.dotfiles

# Load paths
source $root_dir/bash_include/paths
source $root_dir/bash_include/paths.local

export BASH_IT_THEME='minimal'

unset MAILCHECK

source $BASH_IT/bash_it.sh

source $root_dir/bash_include/exports
source $root_dir/bash_include/exports.local
source $root_dir/bash_include/aliases
source $root_dir/bash_include/aliases.local
source $root_dir/bash_include/functions
source $root_dir/bash_include/functions.local
source $root_dir/bash_include/init
source $root_dir/bash_include/init.local
