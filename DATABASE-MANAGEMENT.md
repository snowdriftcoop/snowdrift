Additional Notes about Snowdrift Databases
==========================================

Database migrations
-------------------

NB: Our migration system needs work, see
[SD-501](https://snowdrift.coop/p/snowdrift/w/en/coding/c/2582). The following
describes the way things work as of January 2016:

Some changes to the database schema in config/models are considered *unsafe*
(i.e. changes that could result in data loss, such as dropping a table). When
you make any such changes, recompile, and then start the server, it
automatically generates content in migrations/migrate.unsafe, and the server
will abort.

* If you *don't* want to lose the data (e.g. a column is being moved to a
  different table, a column is being renamed, etc.), alter the contents of
  migrations/migrate.unsafe to be the appropriate SQL commands to preserve the
  data in the desired manner.
* If you *intend* to lose the data (e.g. destroying a column storing data we
  no longer want), the automatic entry in migrate.unsafe can be kept.

Once you are sure about the migration commands working as you intend, rename
migrations/migrate.unsafe to migrations/migrateN (where N is a higher number
than any previous migrate files).

With valid migration files in place, the server should run.

### Committing database migrations

Assuming you are happy with unsafe migrations and ready to commit, first
consolidate multiple uncommitted migration files to only one file per commit.
Then, reset your database (described below) and run the site again to make sure
everything works as expected.

Make sure to add the associated migration file to git when you commit
the corresponding schema changes.

If you try a change that requires unsafe migrations and decide to go a different
direction before committing everything, you will need to both reset your
database and remove the unwanted migration files (as well as reset the relevant
code).

## Database management with the sdm tool

Our sdm tool has several functions for database management.

By default, sdm is set to work with GNU/Linux systems. All instructions here use
that default form. For the other systems listed below, add the given arguments
at the end of any sdm command. When including any arguments, `--` must always be
included after `stack exec` as `stack exec --`.

* OpenBSD: `--sudoUser _postgresql`.
* FreeBSD: `--sudoUser pgsql --pgUser pgsql`
* OS X: `--sudoUser=_postgres`

**All commands below should be run from your snowdrift project directory.**

### Resetting or updating your development database

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit) run:

    stack exec sdm reset

### Sharing updates to the devDB database

If you make specific improvements or additions to your database that you think
will make for a better start for other contributors, use the following command
to export the changes:

    stack exec -- sdm export --db=dev

NB: that command is the same as running the manual postgres command:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

Test that the export worked by running the reset command above and verifying in
the running site that everything works as expected.

Then, the new devDB.sql file may be committed and shared like other changes.

### Updating to the latest test database

If the testDB.sql file gets updated, update your template DB via:

    stack exec -- sdm reset --db=test


Manual database management
--------------------------

The sdm script can do all the common tasks for database management, but here we
explain what they do and how to handle databases manually.

***

### Notes for OS X

Assuming the postgres server is running, where `sudo -u postgres psql` is seen
below, run `psql postgres` instead. The commands that don't use psql can be
adapted to run within the psql command line.

For OS X, instead of `sudo -u postgres psql snowdrift_development <devDB.sql`
follow these steps:

1) Run `psql snowdrift_development`
2) At snowdrift_development=# prompt, run `\i devDB.sql`

Similar adjustments are needed for test database setup and resetting databases.

***

### Setting up the development database manually

Copy the config/postgresql.template to a new config/postgresql.yml file:

    cp config/postgresql.template config/postgresql.yml

Create database user called "snowdrift_development"
*without* superuser, createdb, or createuser privileges:

    sudo -u postgres createuser -S -D -R snowdrift_development

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=#

(NOTE: all of the commands run from the postgres shell must end with a `;`)

Create snowdrift_development database:

    postgres=# create database snowdrift_development;

Add a password to the snowdrift_development user
(for reference, the sdm script generates a random passphrase for this step;
you may substitute any arbitrary passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Leave postgres (with ctrl-D).

Edit config/postgresql.yml and update the password to match the one you entered.

Import development database:

    sudo -u postgres psql snowdrift_development < devDB.sql


### Reset the development database manually

Start by deleting your database:

    sudo -u postgres psql <<<'drop database snowdrift_development'

Then simply re-create the database by rerunning two of the commands
from the "Setting up" section above.

First the "Create snowdrift database" command:

    sudo -u postgres createdb snowdrift_development

and then the "Import development database" command:

    sudo -u postgres psql snowdrift_development <devDB.sql

That's it. You will *not* need to re-run the database user commands.

### Setting up the test template database manually

Like setting up the original development database,
we need to set up a database and user for testing.

Create database user *without* superuser or createrole privileges
but *with* createdb privileges:

    sudo -u postgres createuser -S -d -R snowdrift_test

Create the snowdrift_test database *template*:

    sudo -u postgres createdb snowdrift_test_template

Run postgres psql to bring up the postgres=# prompt:

    sudo -u postgres psql

At the postgres=# prompt, mark the new database as a template:

    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';

Then, add any arbitrary passphrase to the snowdrift_test user
(substitute whatever you like instead of somepassphrase):

    postgres=# alter user snowdrift_test with encrypted password 'somepassphrase';

Leave postgres (with ctrl-D).

If you used a different password than the one you used for
snowdrift_development, then edit config/postgresql.yml and add a "password:"
line under "Testing:" along with your new passphrase.

Finally, import the testDB.sql to the new template database:

    sudo -u postgres psql snowdrift_test_template <testDB.sql

### Resetting the testDB manually

Go to the postgres=# prompt:

    sudo -u postgres psql

Unmark the template (don't include the postgres=# prompt part):

    postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';

Use ctrl-D to leave the prompt.

Drop the template DB:

    sudo -u postgres psql <<<'drop database snowdrift_test_template'

Then we repeat the commands above for setting up the test DB,
skipping the dependencies/user-creation/password parts (those don't need updating).

    sudo -u postgres createdb snowdrift_test_template
    sudo -u postgres psql
    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';
    sudo -u postgres psql snowdrift_test_template <testDB.sql
