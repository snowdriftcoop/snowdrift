# If ".sdc" is being called from ".launch", which it usually is, we will have
# already included "config" back in ".launch". "clusterDir" is only defined
# in "config", so that's how we'll tell whether we need to do the inclusion.
ifndef clusterDir
include config
endif

# This file is created when the cluster gets initialized.
waitForThis := $(clusterDir)/PG_VERSION

# `pg_isready -d foo` tells us whether database foo is ready to accept a
# connection. Below, "-q" is for "quiet": we only look at the program's exit
# status, so we don't want any stdout/stderr here. Note the below is not a
# complete command, it's intended to be coupled with a database name.
# For example: `dbIsReady foo` expands out to "pg_isready -q -d foo".
dbIsReady := pg_isready -q -d

# "Help" is listed first, so it's the default option taken if
# no args are given.
.PHONY: help
help:
	@echo -e "\
	This program is for starting, stopping and wiping out the Snowdrift database\n\
	cluster.\n\
	\n\
	Usually, "launch" runs this program, not a person, because this program only\n\
	concerns the underlying Postgres server, not the whole website.\n\
	\n\
	Usage:\n\
	\n\
	     sdc ACTION\n\
	\n\
	Where ACTION may be one of:\n\
	\n     start             start the cluster\
	\n\
	\n     stop              stop the cluster\
	\n\
	\n     clean             \"rm -rf\" the whole cluster\n\
	                           Note: this also terminates the\n\
	                           the server process.\
	\n\
	\n     help              print this text\n"


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
	@rm -rf $(clusterDir)


######    Documented commands above, implementation-only rules below.    #######


.PHONY: service
service: $(waitForThis)
	## Ensuring database server is running...
#	The initdb command creates the special database "postgres". So "postgres"
#	represents the whole cluster: if we can connect to it, the cluster is
#	running. If we can't, we assume that's because the cluster is not running,
#	in which case we run the start command:
	@$(dbIsReady) postgres || ./.pgStart $(clusterDir)

$(waitForThis): $(clusterDir)
	## Initializing cluster...
	@pg_ctl initdb

$(clusterDir):
	@mkdir $(clusterDir)
