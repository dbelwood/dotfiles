#! /usr/bin/env bash

DOTFILES_PATH=~/.dotfiles

packages_to_install=(
    ruby
    golang
    elixir
    git-hub
    clojure
    java
    xkeycaps
    python
    ansible
    bash
    conky
    i3
    emacs
    i3status
    slack
)

install_gem() {
  if [[ $(gem list --local $1 | grep git-up | wc -l) -eq 0 ]]; then
    echo "Install $1"
    gem install $1
    echo "done"
  fi
}

install_package() {
    echo "Install $1"
    sudo apt-get -y install $1
    echo "done"
}

for app in ${packages_to_install[@]}; do
    install_package $app
done

# Back yo sh*t up
echo "Backing up rc files"
files=(.vimrc .zshrc .gitconfig .vimrc.plugins .tmux.conf)
for file in ${files[@]}; do
  if [[ ! -e $file ]]; then
    continue
  fi
  echo "Backing up $file"
  if [[ -h $file ]]; then
    rm -f $file
  else
    rm $file.bak
    mv $file $file.bak
  fi
done
echo "done."

echo "Create local copies"
touch $DOTFILES_PATH/bash_include/aliases.local
touch $DOTFILES_PATH/bash_include/exports.local
touch $DOTFILES_PATH/bash_include/functions.local
touch $DOTFILES_PATH/bash_include/init.local
touch $DOTFILES_PATH/bash_include/paths.local
echo "done"

# Install bash-it
if [[ ! -e ~/.bash_it ]]; then
    echo "Install Bash It"
    git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
    ~/.bash_it/install.sh
fi

# Set gitconfig settings
echo "Link git configuration"
ln -sf $DOTFILES_PATH/gitconfig ~/.gitconfig
echo "done"

# Bootstrap bash
echo "Link bash configuration"
ln -sf $DOTFILES_PATH/bashrc ~/.bashrc

source ~/.bashrc 
echo "done"

# vim
echo "Link vim configuration"
ln -sf $DOTFILES_PATH/vimrc ~/.vimrc
ln -sf $DOTFILES_PATH/vimrc.plugins ~/.vimrc.plugins
touch ~/.vimrc.local
echo "done"

# emacs
echo "Configure Emacs"
mv ~/.emacs.d ~/.emacs.d.bak
ln -sf $DOTFILES_PATH/emacs.d ~/.emacs.d
echo "Install Cask"
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "Run Cask"
cask --path ~/.emacs.d install
echo "done"

echo "Setup workspace"
mkdir -p $PROJECT_PATH
echo "done"

source ~/.bashrc

echo "Setup golang workspace"
mkdir -p $GOPATH/bin
mkdir -p $GOPATH/pkg
mkdir -p $GOPATH/src
mkdir -p $GOPATH/src/github.com/trunkclub
mkdir -p $GOPATH/src/github.com/dbelwood
echo "done"

echo "Setup other languages"
languages=(Clojure Elixir Ruby Erlang Python)
for language in ${languages[@]}; do
  mkdir -p $PROJECT_PATH/$language
done
echo "done"

echo "All done!"
