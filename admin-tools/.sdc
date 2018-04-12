include config

# This file is created when the cluster gets initialized.
waitForThis := $(clusterDir)/PG_VERSION

# `pg_isready -d foo` tells us whether database foo is ready to accept a
# connection. Below, "-q" is for "quiet": we only look at the program's exit
# status, so we don't want any stdout/stderr here. Note the below is not a
# complete command, it's intended to be coupled with a database name.
# For example: `dbIsReady foo` expands out to "pg_isready -q -d foo".
dbIsReady := pg_isready -q -d

# Some flags used below are documented under the "postgres" command, and
# not directly under "pg_ctl", which inherits from "postgres".
# Those flags let us avoid having a config file. In particular, we use "-k" to
# place the lock file within the project dir. By default, Postgres would try to
# put it somewhere we might not have permission to.
pgStart := pg_ctl start -w -o "-F -h '' -k $(clusterDir)" -l $(clusterDir)/log

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
#	Only stop it if it's ready. If it's not ready, assume it's stopped.
	@ -$(dbIsReady) postgres && pg_ctl stop

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
	@$(dbIsReady) postgres || $(pgStart)

$(waitForThis): $(clusterDir)
	## Initializing cluster...
	@pg_ctl initdb

$(clusterDir):
	@mkdir $(clusterDir)
