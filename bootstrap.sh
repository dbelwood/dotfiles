#! /usr/bin/env zsh

# Install brew
if [ $(which brew | grep 'not found' | wc -l) -eq 1 ]; then
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

is_not_brewed() {
  if [[ -n "$2" ]]; then
    test $(brew cask info $1 | grep 'Not installed' | wc -l) -eq 1
  else
    test $(brew ls --versions $1 | wc -l) -eq 0
  fi
}

# Install brew formulae
echo "Installing brew formulas"
apps=(brew-cask chruby go git hub ruby-install task)
for app ($apps); do
  if is_not_brewed $app; then
    brew install $app
  fi
done
echo "done"

# Install casks
echo "Installing brew casks"
cask_apps=(dropbox flux google-chrome iterm2 macvim)
for app ($cask_apps); do
  if is_not_brewed $app 1; then
    brew cask install $app
  fi
done
echo "done"

# Back yo sh*t up
echo "Backing up rc files"
files=(.vimrc .zshrc .gitconfig .vimrc.plugins)
for file ($files); do
  if [[ ! -e $file ]]; then
    continue
  fi 
  echo "Backing up $file"
  if [[ -h $file ]]; then
    rm -f $file
  else
    mv $file $file.bak
  fi
done
echo "done."

# Install oh-my-zsh
if [[ ! -e ~/.oh-my-zsh ]]; then
  echo "Install Oh My ZSH"
  curl -L http://install.ohmyz.sh | sh
  echo "done"
fi

# Set gitconfig settings
echo "Link git configuration"
ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
echo "done"

# Bootstrap zsh
echo "Link zsh configuration"
ln -sf ~/.dotfiles/zshrc ~/.zshrc

source ~/.zshrc
echo "done"

# Ruby installation
local ruby_ver="2.1.5"
echo "Install chruby and ruby version $ruby_ver"
ruby-install ruby $ruby_ver # Install a system ruby
chruby ruby-$ruby_ver # Set system ruby
gem install up # Install up
echo "done"

# vim
echo "Link vim configuration"
ln -sf ~/.dotfiles/vimrc ~/.vimrc
ln -sf ~/.dotfiles/vimrc.plugins ~/.vimrc.plugins
echo "done"

env zsh
. ~/.zshrc
echo "All done!"
