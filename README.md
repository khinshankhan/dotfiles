# dotfiles

My dotfiles. They may help you, but they mostly help me :thinking:

## Install

On a fresh machine, `bootstrap` installs the prerequisites (git + stow), clones
the repository, and stows everything. It needs only curl to start:

```bash
curl -fsSL https://raw.githubusercontent.com/khinshankhan/dotfiles/main/bootstrap | bash
```

Already cloned? Manage packages with [`dottie`](./dottie) (requires [GNU
Stow](https://www.gnu.org/software/stow/)):

```bash
dottie prestow              # set up directories to avoid symlink ownership conflicts
dottie install              # stow all packages
dottie install emacs git    # stow specific packages
```

## Contributing

Bug reports and PRs are welcome. Please open an issue first for discussion.

Feel free to open an issue if you spot something iffy or have a hot tip :shrug:

## License

Apache-2.0. See [LICENSE](./LICENSE). If applicable, see [NOTICE](./NOTICE).
