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

## Database management with the sdb tool

Our sdb tool has several functions for database management.

**The below assumes you have added the `admin-tools` subdirectory to your shell's
`PATH` variable.** You may also want to add `dev-tools`. Make sure you have
nothing overriding `sdb.sh` by running `type sdb.sh`. Verify that the path is
what you expect.

### Resetting or updating your development database

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit) run:

    sdb.sh clean

### Sharing updates to the devDB database

If you make specific improvements or additions to your database that you think
will make for a better start for other contributors, use the `sdb.sh env`
command to get the code needed.

*TODO: add instructions for how to use those commands for those unfamiliar*

Test that the export worked by running the reset command above and verifying in
the running site that everything works as expected.

Then, the new devDB.sql file may be committed and shared like other changes.

### More database operations

To see all the commands sdb.sh supports, run:

    sdb.sh help

### Database directory location

At time of writing, it's right in the project root. If you desire, it should be
pretty easy to change where it goes. Please see comments in sdb.sh for details.
The directory is `.postgres-work`.

Note, however, that you are getting into developers' territory if you do this:
moving your database directory is not yet a documented process. Do you need to
run `sdb.sh stop` first? Are you going to copy your stuff over? Should you
update .gitignore? Etc.

### sdb Dev Notes

* The user runs sdb.sh, which then runs the core executable. The parameters are
propagated.

* We provide ./sdb-fallback.sh in case there is a problem with build.sh. The
fallback script locally builds the core sdb executable and then launches it,
passing on its parameters. Note it compiles sdb.hs every run.

* The script, sdb.sh, is well-commented. It should be easy to
maintain/troubleshoot, using those comments.
