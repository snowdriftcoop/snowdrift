# Assured Executables

**WARNING: this is all experimental and untested.**

We may use the Nix package manager to ensure certain needed programs are
installed and visible to what needs them.

To do so:
* Install the Nix shell,
* Un-comment the `SHELL := nix-shell` line in the `nix` file,
* Add your needed packages to the `packages` variable in `nix`,
* Run your needy programs through a Makefile that includes `nix`.
  * This will run them in a Nix shell environment where the specified packages
  have been installed.
  * You might simply "include" `config`, which includes `nix`.
