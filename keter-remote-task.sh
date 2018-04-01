#!/usr/bin/env bash

#
# keter-remote-task.sh: executed on a remote build server by keter-remote.sh.
#

set -e

log () {
    echo -n ":::: (worker) "
    echo $@
}

main () {
    if [ $# -ne 2 ]
    then
        log "Incorrect worker arguments: [$*]"
        log "Expected: [BUILDREV APPNAME]"
        exit 1
    fi
    BUILDREV=$1
    export APPNAME=$2

    mkdir -p keter-remote-task
    cd keter-remote-task
    if [ ! -d snowdrift ]
    then
        log "Cloning repo"
        git clone https://git.snowdrift.coop/sd/snowdrift.git
        cd snowdrift
    else
        cd snowdrift
        log "Fetching repo"
        git fetch
    fi
    log "Switching to build rev ${BUILDREV}"
    git reset --hard ${BUILDREV}
    log "Building deployment bundle"
    DEPLOY=false ./keter.sh
    mv ${APPNAME}.keter ../..
}
main $@
