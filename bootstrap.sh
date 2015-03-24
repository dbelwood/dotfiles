#! /usr/bin/env zsh

DOTFILES_PATH=~/.dotfiles
brews_to_install=(
  brew-cask
  chruby
  go
  git
  hub
  ruby-install
  task
  tmux)
brew_casks_to_install=(
  dropbox
  flux
  google-chrome
  iterm2
  macvim)

install_gem() {
  if [[ $(gem list --local $1 | grep git-up | wc -l) -eq 0 ]]; then
    echo "Install $1"
    gem install $1
    echo "done"
  fi
}

# Install brew
if [ $(which brew | grep 'not found' | wc -l) -eq 1 ]; then
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Install brew formulae
echo "Installing brew formulas"
for app ($brews_to_install); do
  if [[ $(brew ls --versions $app | wc -l) -eq 0 ]]; then
    brew install $app
  fi
done
echo "done"

# Install casks
echo "Installing brew casks"
for app ($brew_casks_to_install); do
  if [[ $(brew cask info $app | grep 'Not installed' | wc -l) -eq 1 ]]; then
    brew cask install $app
  fi
done
echo "done"

# Back yo sh*t up
echo "Backing up rc files"
files=(.vimrc .zshrc .gitconfig .vimrc.plugins .tmux.conf)
for file ($files); do
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
touch $DOTFILES_PATH/zsh_include/aliases.local
touch $DOTFILES_PATH/zsh_include/exports.local
touch $DOTFILES_PATH/zsh_include/functions.local
touch $DOTFILES_PATH/zsh_include/init.local
touch $DOTFILES_PATH/zsh_include/paths.local
echo "done"

# Install oh-my-zsh
if [[ ! -e ~/.oh-my-zsh ]]; then
  echo "Install Oh My ZSH"
  curl -L http://install.ohmyz.sh | sh
  echo "done"
fi

# Set gitconfig settings
echo "Link git configuration"
ln -sf $DOTFILES_PATH/gitconfig ~/.gitconfig
echo "done"

# Bootstrap zsh
echo "Link zsh configuration"
ln -sf $DOTFILES_PATH/zshrc ~/.zshrc

source ~/.zshrc
echo "done"

# Ruby installation
ruby_ver=2.1.5
if [[ $(chruby | grep $ruby_ver | wc -l) -eq 0 ]]; then
  echo "Install ruby version $ruby_ver"
  ruby-install ruby $ruby_ver # Install a system ruby
fi

echo "Set system ruby version"
chruby ruby-$ruby_ver # Set system ruby

install_gem git-up # Install git-up

# vim
echo "Link vim configuration"
ln -sf $DOTFILES_PATH/vimrc ~/.vimrc
ln -sf $DOTFILES_PATH/vimrc.plugins ~/.vimrc.plugins
echo "done"

# tmux
echo "Link tmux configuration"
ln -sf $DOTFILES_PATH/tmux.conf ~/.tmux.conf
install_gem teamocil #install teamocil
ln -sf $DOTFILES_PATH/teamocil_layouts ~/.teamocil
echo "done"

install_gem teamocil # Tmux config

echo "Install teamocil layouts"
cp teamocil_layouts/*.yml ~/.teamocil
echo "done"

echo "Setup workspace"
mkdir -p $PROJECT_PATH
echo "done"

source ~/.zshrc

echo "Setup golang workspace"
mkdir -p $GOPATH/bin
mkdir -p $GOPATH/pkg
mkdir -p $GOPATH/src
mkdir -p $GOPATH/src/github.com/trunkclub
mkdir -p $GOPATH/src/github.com/dbelwood
echo "done"
echo "All done!"
