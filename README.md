# dotfiles

My dotfiles. They may help you, but they mostly help me :thinking:

## Install

On a fresh machine, `bootstrap` installs the prerequisites (git + stow), clones
the repository, and installs only the `dottie` command. It needs only curl to start:

```bash
curl -fsSL https://raw.githubusercontent.com/khinshankhan/dotfiles/main/bootstrap | bash
```

Already cloned? Manage packages with [`dottie`](./dottie) (requires [GNU
Stow](https://www.gnu.org/software/stow/)):

```bash
dottie prestow              # set up directories to avoid symlink ownership conflicts
dottie install emacs git    # stow specific packages
dottie install emacs git --dry-run # preview specific packages
dottie install --machine gengar # preview an explicit machine list
dottie install --machine gengar --apply # apply that list
```

Bare `dottie install` prints usage and changes nothing. There is no install-all
mode. Machine installs read `packages/<name>/dotfiles.list`: one Stow package
name per line, with blank lines and `#` comments allowed. Create the list before
using it, for example:

```text
# packages/gengar/dotfiles.list
shell
git
tmux
```

Lists must be nonempty and contain only valid top-level package names. A machine
install previews changes by default; add `--apply` to restow the listed packages.
It does not remove previously installed packages omitted from the list.
Explicit package installs apply by default; `--dry-run` previews them.
`--dry-run` and `--apply` cannot be combined.

## Contributing

Bug reports and PRs are welcome. Please open an issue first for discussion.

Feel free to open an issue if you spot something iffy or have a hot tip :shrug:

## License

Apache-2.0. See [LICENSE](./LICENSE). If applicable, see [NOTICE](./NOTICE).
