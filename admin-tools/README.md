# Administrators' Tools

### Execution Notes

* The executable Makefiles below must be ran from within the project directory
("snowdrift" or deeper). Otherwise, the `stack path --project-root` command will
mislead them with its default answer, and they will not be able to find the
config file.

* We highly recommend adding the absolute path to `dev-tools` to your `PATH`
variable, such as by editing `~/.bash_profile`. That way, you may simply run
`foo [command]` instead of `dev-tools/foo [command]`.

### Editing Notes

* When editing the scripts, please be aware that all recipe lines *must* begin
with a **real tab character**. So you may wish to use a text editor other than
your main development environment if the latter is already set up to use spaces
for tabs.

## Tools

### sdp

Stands for ***S***now***d***rift ***p***rompt, i.e, a `psql` prompt open to a
local Snowdrift database.

This command is a convenience launcher for `psql`, passing in the cluster
connection parameter for you. Usually, it specifies the database for you as
well. This is determined by the `defaultDb` variable in the `config` file.
Alternatively, you may run `sdp db=[database name]` if you wish to
override that behavior without changing the config file.

If you have any other args you wish to pass to `psql`, such as `--single-step`,
simply do `sdp args="--single step"`. The quotes enable multiple space-separated
arguments, like `sdp args="--single-step --tuples-only"`.

### sdsh

Stands for ***S***now***d***rift ***sh***ell. It launches the Snowdrift site and
then gives you a Bash prompt via `stack exec`, so your previous
`stack exec -- foo` commands may be simplified to just `foo` while within this
shell. It sets `PGHOST`, `PGDATA` and `PGDATABASE` environment variables for
you, too.

### sdc

This program is normally ran by `launch`, not directly by a person. The `launch`
program starts the whole website. `sdc` only touches the underlying
PostgreSQL portion.

`sdc` stands for ***S***now***d***rift ***c***luster. For example, `sdc start`
stands for "Snowdrift cluster, start". At time of writing, the cluster contains
a main database and a test one. The program can also stop or clean the cluster.
Please run `sdc help` for more info. Its `clean` command, used by people, is how
we categorize it as a "tool".

## Configuration

The above tools and `launch` use the `config` file in this directory to:

* Set environment variables needed by Yesod
* Set the name and location of the cluster directory
* Add to the `PATH` variable
* Know the project root path
* Give abstract names to database names, like "mainDb" for "snowdrift"
* Set the defaultDb
* Optionally use the Nix package manager. Please see EXECUTABLE-ASSURANCE.md.
