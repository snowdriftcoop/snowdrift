# Administrators' Tools

### Execution Notes

* We highly recommend adding the absolute path to `admin-tools` to your `PATH`
variable, such as by editing `~/.bash_profile`. This way, you may simply run
`foo [command]` instead of `admin-tools/foo [command]`.

### Editing Notes

* When editing the scripts, please be aware that all recipe lines *must* begin
with a **real tab character**. So you may wish to use a text editor other than
your main development environment if the latter is already set up to use spaces
for tabs, unless you know it will recognize the file is tab-indented and act
accordingly.

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
* Give abstract names to database names, like "mainDb" for "snowdrift"
* Set the defaultDb
* Optionally use the Nix package manager. Please see EXECUTABLE-ASSURANCE.md.

## "Dot" things

Recall that our executable Makefiles all use `config`. Them all sharing this
file is much better than them each having an identical prelude that does what
"including" `config` does.

But this gives us a challenge: how do we specify the path to `config` in order
to `include` it? What if this path has spaces in it? How do we escape spaces
in a Makefile? Well, actually, we don't.

Instead, we use "dot" things:

* in `admin-tools`, we have a `.foo` file for every executable `foo`,
* define `clusterDir` as `../postgres-work` (at time of writing) in `config` and
* use a hidden Bash script, `.pgStart`.

In Make, unlike Bash, one cannot simply escape a file path by wrapping it in
single quotes. Make gives us two options here:

1. Use an ugly string manipulation command to escape characters according to
Make's special character rules (note we must do this programmatically because
we cannot anticipate where on your drive you will place your `snowdrift`
repository clone), or

2. `cd` to where our stuff is and use relative directories.

I don't want that ugly code in every executable Makefile, which is what it would
take to include `config` if we took option one. So I went with option two. This
requires an intermediate launcher script to `cd` to the location of itself,
which then launches the Makefile, which is in the same directory: `admin-tools`.
The launcher is `foo` and the Makefile is `.foo`. This way, you don't even have
to `cd` into `admin-tools`: the launcher script does that for you based off its
own location.

As far as `../postgres-work`, `..` is a special directory with no special
characters in its name. It is a kernel-facilitated link to the parent directory.

However, `pg_ctl` requires its socket directory parameter to be given as an
absolute path, as least when the `-w` option is present, which it is for us.

This is all handled in `.pgStart`, which is only ran by `.sdc`. The former is a
Bash script. Critically, in Bash, we can easily escape any special characters
that appear in the absolute path. I believe this is impossible to do in Make
because it offers no quoting mechanism for escaping, nor does it allow you to
escape a backslash. And note: if one were to attempt this in Make, it'd have to
be done differently for what shell Make is set to use. This must be considered
because we give the option of either the default shell or the Nix shell. They
have different ways of escaping file paths.
