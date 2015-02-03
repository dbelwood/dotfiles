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
apps=(brew-cask chruby go git hub ruby-install task)
for app ($apps); do
  if is_not_brewed $app; then
    brew install $app
  fi
done

# Install casks
cask_apps=(dropbox flux google-chrome iterm2 macvim)
for app ($cask_apps); do
  if is_not_brewed $app 1; then
    brew cask install $app
  fi
done

# Back yo sh*t up
files=(.vimrc .zshrc .gitconfig .vimrc.plugins)
for file ($files); do
  if [[ -h $file ]]; then
    rm -f $file
  else
    mv $file $file.bak
  fi
done

# Install oh-my-zsh
if [[ -z ~/.oh-my-zsh ]]; then
  curl -L http://install.ohmyz.sh | sh
fi

# Set gitconfig settings
ln -sf ~/.dotfiles/gitconfig ~/.gitconfig

# Bootstrap zsh
ln -sf ~/.dotfiles/zshrc ~/.zshrc

source ~/.zshrc

# Ruby installation
#ruby-install ruby 2.1.5 # Install a system ruby
chruby ruby-2.1.5 # Set system ruby
gem install up # Install up

# vim
ln -sf ~/.dotfiles/vimrc ~/.vimrc
ln -sf ~/.dotfiles/vimrc.plugins ~/.vimrc.plugins

env zsh
. ~/.zshrc
echo "All done!"
