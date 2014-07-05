# Miscellaneous Dotfiles and Configuration Files

## Emacs
I specifically do not commit emacs plugins.

Unfortunately this results in a bit of wasted time when bringin up a new environment:

```
M-x load-file .emacs
```
See error message, then:

```
M-x package-list-packages
```
Install the missing package.

**repeat**

## Tmux
I don't commit Tmux plugins.

A usual first-time setup goes like this:

```
cd ~/.tmux/plugins
rm tpm
git clone <tpm git url>
```
Now quit all tmux sessions.

Start tmux: `tmux`
Initialize plugins: `ctrl+I`

## .config
This is not a full clone of any of my `.config` directories.
Rather, I cherry-pick which config directories I want to commit.

So on a first install, I'll do something like:

```
ln -s ~/code/dotfiles/.config/i3 ~/.config/i3
```
## Bash
I fairly heavily use Bash specifically, as opposed to sh or some other dialect.

I have a `.profile`, `.bash_profile` and `.bashrc`.  All of these should be symlinked.

`.bash_profile` is used when running non-interactive bash shells, which Ubuntu does via `dash`.

`.bash.d` should be symlinked, but I also expect to find `~/.backup/.termbg` like so:

```
#~/.backup/.termbg
dark
```
This is probably not really necessary anymore. It was primarily used for switching between light and dark color schemes in the terminal.

**Emacs** is also reliant on the `~/.backup` directory.