# Install brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install brew formulae
apps=(brew-cask chruby go git hub ruby-install task)
for app ($apps); do
  brew install $app
done

# Install a system ruby
ruby-install ruby 2.1.5
chruby ruby-2.1.5

# Install up
gem install up

# Install casks
cask_apps=(dropbox flux google-chrome iterm2 macvim)
for app ($cask_apps); do
  brew cask install $app
done

# Install oh-my-zsh
rm -rf ~/.oh-my-zsh # Clear out old install
curl -L http://install.ohmyz.sh | sh

# Bootstrap home directory
# zsh
cp -rf .zsh* ~

# vim
cp -rf .vim* ~

source ~/.vimrc

echo "All done!"
