#!/bin/bash

#
# keter.sh: Like "yesod keter", but works with our split-package project.
#

#
# Options
#

opt_build=${BUILD:=true}
opt_deploy=${DEPLOY:=true}

#
#
#

install_path=./dist/bin

contents=(
    config
    static
    migrations
    templates
    ops
    dist
)

sd-main-dns () {
    aws --profile snowdrift ec2 describe-instances \
        --instance-ids i-81a6df28 \
        --query 'Reservations[].Instances[].PublicDnsName'
}

hdr () {
    echo -e "\n-- $@"
}

main () {
    if $opt_build
    then
        hdr "Building"
        rm -fr dist/bin
        mkdir -p dist/bin
        stack --local-bin-path $install_path install --flag Snowdrift:-dev
        hdr "Packing executables"
        upx dist/bin/Snowdrift*
    else
        hdr "Not building, as requested"
    fi
    rm -rf static/tmp/*
    hdr "Tarballing"
    tar czf Snowdrift.keter ${contents[@]}
    if $opt_deploy
    then
        hdr "Deploying"
        scp Snowdrift.keter `sd-main-dns`:/opt/keter/incoming
    else
        hdr "Not deploying, as requested"
    fi
}

main
