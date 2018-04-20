# If we're using Nix's shell, we pass the below flags to nix-shell.
# This is to ensure the listed packages are installed and visible.
ifeq "$(SHELL)" "nix-shell"
    .SHELLFLAGS := --packages postgresql --run
endif

mainDb := $(PGDATABASE)
testDb := snowdrift_test

# This file is created after the cluster is initialized:
waitForThis := $(PGDATA)/PG_VERSION

# `pg_isready -d foo` tells us whether database foo is ready to accept a
# connection. Below, "-q" is for "quiet": we only look at the program's exit
# status, so we don't want any stdout/stderr here. Note the below is not a
# complete command; it's intended to be coupled with a database name.
# For example: `dbIsReady foo` expands out to `pg_isready -q -d foo`.
dbIsReady := pg_isready -q -d

# | We do not use a Postgres config file. We use cmd-line flags instead.
# "-k" specifies where to place the Postgres lock file. Otherwise, Postgres
# may try to put it somewhere we don't have permission to. This is documented
# under "unix_socket_directories", which the "-k" doc references.
# NB: some flags below are *not* documented under `pg_ctl`, but under the
# `postgres` command, from which the former inherits.
pgStart := pg_ctl start -w -o "-F -h '' -k $(PGHOST)" -l $(PGDATA)/log

# First, start the service (postgres server backend program), then ensure
# the desired databases exist.
.PHONY: start
start: service
	## Ensuring our databases exist...
#	If the main Snowdrift database is ready, don't create it.
#	If it is not ready, we assume this is because it does not exist.
#	So if it's the latter, we create it. Otherwise, skip the creating.
	@$(dbIsReady) $(mainDb) || createdb $(mainDb)
#	And same for the test database:
	@$(dbIsReady) $(testDb) || createdb $(testDb)

.PHONY: stop
stop:
#   The "|| echo" ensures this command is counted as a success. We're not using
#   Make's "-" prefix because that still gives a confusing, purportedly
#   "ignored" error. If the database can't be stopped because it's already
#   stopped, then we consider this a vacuous success.
	@pg_ctl stop || echo

.PHONY: clean
clean: stop
	@rm -rf $(PGDATA)


######    Documented commands above, implementation-only rules below.    #######


.PHONY: service
service: $(waitForThis)
	## Ensuring database server is running...
#	The initdb command creates the special database "postgres". So "postgres"
#	represents the whole cluster: if we can connect to it, the cluster is
#	running. If we can't, we assume that's because the cluster is not running,
#	in which case we run the start command:
	@$(dbIsReady) postgres || $(pgStart)

$(waitForThis): $(PGDATA)
	## Initializing cluster...
	@pg_ctl initdb

$(PGDATA):
	@mkdir $(PGDATA)
