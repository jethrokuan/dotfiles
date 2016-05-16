# Jethro's dotfiles

# Install
- clone the repository : `git clone https://github.com/jethrokuan/dotfiles.git ~/`
  *Important:* it must be cloned in your `$HOME` for *.stowrc* files to work as
  expected
- execute `stow stow` first to install stow global ignore file

Then, edit around and stow away!

## Guidelines

### :one: Directories naming rules

Few simple rules to make the nature of stow packages explicit :

- lowercase for packages to install in `$HOME`
- titlecase for packages to install as root in `/`, eg
  [`@Daemon-osx`](https://github.com/Kraymer/F-dotfiles/blob/master/attic/@Daemon-osx)
- leading `@` for environment packages and subpackages, eg
  [`@mac`](https://github.com/Kraymer/F-dotfiles/blob/master/%40mac/), [`attic/@Daemon-osx`](https://github.com/Kraymer/F-dotfiles/blob/master/attic/@Daemon-osx)
- leading `_` for non packages, eg [`_homebrew`](https://github.com/Kraymer/F-dotfiles/blob/master/_homebrew)

### :two: Documentation

Each package has a `README.md` that introduces the package features.
