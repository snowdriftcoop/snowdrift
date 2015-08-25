Additional Notes about Snowdrift Databases
==========================================

Database migrations
-------------------

NOTE: THE MIGRATION SITUATION IS BEING ADJUSTED, THIS MAY BE OUTDATED

After any change to the database schema (in config/models),
the first time you recompile and then start the server,
a migration script will be automatically generated and placed in /migrations.

If there are no unsafe statements in the migration,
the migrations will be run and the server will continue to start normally.

If there are any unsafe (may destroy data) statements,
they are placed in migrations/migrate.unsafe, and the server will abort.

In an unsafe case, if the data *is* intended to be lost
(e.g. destroying a column storing data we no longer want),
copy the statements to the new migrateN file (creating it if necessary).

If you don't want to lose the data
(a column is being moved to a different table, a column is being renamed, &c),
modify the migration file to use the appropriate intended SQL commands.

### Committing database migrations

In the course of testing and/or resetting your database,
you might generate extra migrations. When that happens,
be sure to reset your database andremove any extraneous migration files.
Once you have a final version of the code, you can run the site once to
generate the correct final migration.

Ideally consolidate all migrations to only one migration file per commit.

Make sure to add the associated migration file to git when you commit
the corresponding schema changes.

When merging migrations, put any you've added on the end in separate file(s).
Don't merge them into migration files others may have already run.


Resetting or updating your development database
-----------------------------------------------

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit) run:

    sdm reset

Sharing updates to the devDB database
-------------------------------------

If you make specific improvements or additions to your database
that you think will make for a better start for other contributors
(and also when you have updated the basic database with migration files),
you can use the following command to export the changes
(which can then be committed via git as usual).

While in your project directory:

    stack exec -- sdm export --db=dev

which is the same as running:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

You can test that the export worked by running `sdm reset` and verifying
in the running site that everything is as expected.

Then, the new devDB.sql file may be committed and shared like other changes.

Updating to the latest test database
------------------------------------

If the testDB.sql file gets updated, you'll need to update your template.

Simply run `stack exec -- sdm reset --db=test` to reset/update your test databases.


Manual database management
--------------------------

Our sdm script makes database management quick and easy.
All the steps below can be done simply with the sdm script,
but here we explain what it does and how to handle databases manually.

***

### Notes for OS X

Assuming the postgres server is running, where `sudo -u postgres psql` is seen
below, run `psql postgres` instead. The commands that don't use psql can be
adapted to run within the psql command line.

For OS X, instead of `sudo -u postgres psql snowdrift_development <devDB.sql`
follow these steps:

1) Run `psql snowdrift_development`
2) At snowdrift_development=# prompt, run `\i devDB.sql`

Similar adjustments will be needed for the
test database setup and resetting databases.

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
