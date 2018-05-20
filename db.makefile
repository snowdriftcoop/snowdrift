# If we're using Nix's shell, we pass the below flags to nix-shell.
# This is to ensure the listed packages are installed and visible.
ifeq "$(SHELL)" "nix-shell"
    .SHELLFLAGS := --packages postgresql --run
endif

mainDb := $(PGDATABASE)
testDb := snowdrift_test

# This file is created after the cluster is initialized.
# It remains after the `stop` command.
isInit := $(PGDATA)/PG_VERSION

# https://stackoverflow.com/a/16783253/994643
# Note the below is not a complete command; it's intended to be coupled with a
# database name. For example: `dbExists foo` expands out to
# `psql -lqt | cut -f1 -d \| | grep -qw foo`. Also note it does not print
# anything; we only use the exit status.
dbExists := psql -lqt | cut -f1 -d \| | grep -qw

# We just want the exit status: could `psql` connect to the cluster?
clusRunning := psql -l &> /dev/null

# | We do not use a Postgres config file. We use cmd-line flags instead.
# "-k" specifies where to place the Postgres lock file. Otherwise, Postgres
# may try to put it somewhere we don't have permission to. This is documented
# under "unix_socket_directories", which the "-k" doc references.
# NB: some flags below are *not* documented under `pg_ctl`, but under the
# `postgres` command, from which the former inherits.
pgStart := pg_ctl start -w -o "-F -h '' -k $(PGHOST)" -l $(PGDATA)/log

.PHONY: start
start: service
	## Ensuring our databases exist...
	$(dbExists) $(mainDb) || createdb $(mainDb)
	$(dbExists) $(testDb) || createdb $(testDb)

.PHONY: stop
stop:
#   The "|| echo" ensures this command is counted as a success. We're not using
#   Make's "-" prefix because that still gives a confusing, purportedly
#   "ignored" error. If the cluster can't be stopped because it's already
#   stopped, then we consider this a vacuous success.
	pg_ctl stop || echo

.PHONY: clean
clean: stop
	rm -rf $(PGDATA)


######    Documented commands above, implementation-only rules below.    #######


.PHONY: service
service: $(isInit)
	## Ensuring database server is running...
	$(clusRunning) || $(pgStart)

$(isInit):
	## Initializing cluster...
	pg_ctl initdb
