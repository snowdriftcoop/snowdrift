##
## ARGUMENTS AND VARIABLES
##

# This flag causes all commands to run in a nix shell.
# Example:
#
#     USE_NIX=1 make -f db.makefile
#
ifdef USE_NIX
    SHELL := nix-shell
    .SHELLFLAGS := --packages postgresql --run
endif

# Set postgres environment. PGDATA and PGDATABASE can be specified by the user.
export PGDATA ?= $(CURDIR)/.postgres-work
export PGDATABASE ?= snowdrift_development
export PGHOST := $(PGDATA)

# This is one of the files created when a cluster is initialized.
cluster_sentinel := $(PGDATA)/PG_VERSION

##
## HELPER COMMANDS
##

pg_isready = pg_isready -q
# TODO: explain these flags
pg_start = pg_ctl start -w -o "-F -h '' -k $(PGHOST)" -l $(PGDATA)/log

# Example usage:
#
#     $(db_exists) foo
#
# Result is returned as exit status.
#
# https://stackoverflow.com/a/16783253/994643
db_exists := psql -lqt | cut -f1 -d \| | grep -qw

##
## GOALS
##

.DEFAULT_GOAL := database

.PHONY: database
database: service ; $(db_exists) $(PGDATABASE) || createdb $(PGDATABASE)

.PHONY: service
service: cluster ; $(pg_isready) || $(pg_start)

.PHONY: cluster
cluster: $(cluster_sentinel)

$(cluster_sentinel): ; pg_ctl initdb -D $(PGDATA)

.PHONY: stop
stop: ; -$(pg_isready) && pg_ctl stop

.PHONY: clean
clean: stop
	rm -rf $(PGDATA)
