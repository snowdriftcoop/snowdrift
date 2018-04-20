Additional Notes about Snowdrift Databases
==========================================

Database migrations
-------------------

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

**The old migration code has been removed, and nothing has replaced it
yet.**  See [Issue 302](https://tree.taiga.io/project/snowdrift/issue/302).

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

## Database management with build.sh

`build.sh` has several functions for database management.

**All commands below should be run from your snowdrift project directory.**

### Resetting or updating your development database

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit) run:

    ./build.sh clean

### Sharing updates to the devDB database

If you make specific improvements or additions to your database that you think
will make for a better start for other contributors...*TODO: add instructions
for how to use those commands for those unfamiliar*

Test that the export worked by running the reset command above and verifying in
the running site that everything works as expected.

Then, the new devDB.sql file may be committed and shared like other changes.

### More database operations

To see all the commands build.sh supports, run:

    ./build.sh help
