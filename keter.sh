#!/bin/bash

set -e

#
# keter.sh: Like "yesod keter", but works with our split-package project.
#

#
# Options
#

opt_build=${BUILD:=true}
opt_deploy=${DEPLOY:=true}
opt_appname=${APPNAME:=SnowdriftReboot}

#
#
#

install_path=./dist/bin

contents=(
    config
    static
    migrations
    ops
    dist
)

hdr () {
    echo -e "\n-- $@"
}

main () {
    if $opt_build
    then
        hdr "Building"
        if [ -z "$install_path" ]; then
            >&2 echo "Hold up, \$install_path should be specified!"
            exit 1
        fi
        rm -rf ${install_path}
        mkdir -p ${install_path}
        stack --work-dir .stack-work-deploy --local-bin-path $install_path install --flag Snowdrift:-dev --pedantic
        hdr "Packing executables"
        find ${install_path} -type f -executable | xargs upx
    else
        hdr "Not building, as requested"
    fi
    rm -rf static/tmp/*
    hdr "Tarballing"
    tar czf ${opt_appname}.keter ${contents[@]}
    if $opt_deploy
    then
        hdr "Deploying"
        scp ${opt_appname}.keter `sd-main-dns`:/opt/keter/incoming
    else
        hdr "Not deploying, as requested"
    fi
}

time main
