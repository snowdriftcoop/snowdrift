#!/usr/bin/env bash

#
# keter-remote.sh: Like keter.sh, this build and deploys Snowdrift. However, it
# hands off the build step to a remote server.
#
# This script exists because I migrated my laptop to NixOS, and could no longer
# build binaries that would run on Ubuntu. This is a temporary problem.
# -Bryan
#

set -e

usage () {
    echo -e "Usage:\n\t""$0"" <REMOTE-HOST>"
    echo
    echo "Optional vars:"
    echo -e "\tAPPNAME \tWhat to call the deployment (default: SnowdriftReboot)"
    echo -e "\tBUILDREV\tWhich revision to deploy (default: HEAD)"
}

cancel_handler () {
    echo ":: Sending cancel commands"
    ssh "$1" 'pkill -u ubuntu stack'
}

main () {
    if [ $# -ne 1 ]
    then
        usage
        exit 1
    elif [ "$1" = "--help" -o "$1" = "-h" ]
    then
        usage
        exit 0
    fi

    HOST="$1"

    : ${APPNAME:=SnowdriftReboot}
    : ${BUILDREV:=$(git rev-parse HEAD)}
    export APPNAME BUILDREV

    echo ":: Running remote task"
    (
        trap "cancel_handler ${HOST}" EXIT
        ssh ${HOST} 'bash -s' -- < ./keter-remote-task.sh ${BUILDREV} ${APPNAME}
        trap - EXIT
    )
    echo ":: Fetching deployment bundle"
    scp ${HOST}:${APPNAME}.keter .
    echo ":: Pushing deployment bundle to prod"
    BUILD=false ./keter.sh
}

cd $(dirname "$0")
main $@
