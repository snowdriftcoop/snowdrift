ifdef USE_NIX
    SHELL := nix-shell
    .SHELLFLAGS := --packages postgresql --run
endif

export PGDATA ?= $(CURDIR)/.postgres-work
export PGDATABASE ?= snowdrift_development
export PGHOST := $(PGDATA)

pg_isready := pg_isready -q
pg_ctl := $(shell pg_config --bindir)/pg_ctl

# TODO: explain these flags
pg_start := $(pg_ctl) \
    start -w -o "-F -h '' -k $(PGHOST)" -l $(PGDATA)/log

# Example usage:  $(db_exists) foo
#
# Result is returned as exit status.
#   - https://stackoverflow.com/a/16783253/994643
db_exists := psql -lqt | cut -f1 -d \| | grep -qw

.PHONY: database service cluster stop clean

database: service ; $(db_exists) $(PGDATABASE) || createdb $(PGDATABASE)

service: cluster ; $(pg_isready) || $(pg_start)

# This is one of the files created when a cluster is initialized.
cluster_sentinel := $(PGDATA)/PG_VERSION
cluster: $(cluster_sentinel)
$(cluster_sentinel): ; $(pg_ctl) initdb -D $(PGDATA)

stop: ; -$(pg_isready) && $(pg_ctl) stop
clean: stop ; rm -rf $(PGDATA)
