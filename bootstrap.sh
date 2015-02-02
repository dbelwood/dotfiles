#! /usr/bin/env zsh

# Install brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

is_brewed() {
  test $(brew ls --versions $1 | wc -l) -eq 1
}

# Install brew formulae
apps=(brew-cask chruby go git hub ruby-install task)
for app ($apps); do
  if [ ! is_brewed $app ]; then
    brew install $app
  fi
done

# Install casks
cask_apps=(dropbox flux google-chrome iterm2 macvim)
for app ($cask_apps); do
  brew cask install $app
done

# Back yo sh*t up
files=(.vimrc .zshrc .gitconfig)
for file ($files); do
  mv $file $file.bak
done

# Install oh-my-zsh
if [[ -z ~/.oh-my-zsh ]]; then
  curl -L http://install.ohmyz.sh | sh
fi

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
