#! /usr/bin/env zsh

# Install brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install brew formulae
apps=(brew-cask chruby go git hub ruby-install task)
for app ($apps); do
  brew install $app
done

# Install casks
cask_apps=(dropbox flux google-chrome iterm2 macvim)
for app ($cask_apps); do
  brew cask install $app
done

# Install oh-my-zsh
rm -rf ~/.oh-my-zsh # Clear out old install
curl -L http://install.ohmyz.sh | sh

# Bootstrap zsh
ln -sf ./zshrc ~/.zshrc

source ~/.zshrc

# Ruby installation
#ruby-install ruby 2.1.5 # Install a system ruby
chruby ruby-2.1.5 # Set system ruby
gem install up # Install up

# vim
ln -sf ./vimrc ~/.vimrc

echo "All done!"
