#!/usr/bin/env bash

set -e

#
# keter.sh: Like "yesod keter", but works with our split-package project.
#

#
# Options
#

opt_build=${BUILD:=true}
opt_deploy=${DEPLOY:=true}
# NB! Whatever name is used here must also be used in the 'crowdmatch' app
# (crowdmatch/app/crowdmatch.hs), where it is used to pull in postgres
# configuration. Must fix.
opt_appname=${APPNAME:=SnowdriftReboot}

#
#
#

install_path=./website/dist/bin

contents=(
    config
    static
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
        stack --work-dir .stack-work-deploy clean
        # Have to do dependencies without --pedantic, since stack still
        # rebuilds extra-deps specified as git repos after a clean. :(
        # Refer to https://github.com/commercialhaskell/stack/issues/1295
        stack \
            --work-dir .stack-work-deploy \
            build \
            --dependencies-only \
            --install-ghc
        stack \
            --work-dir .stack-work-deploy \
            --local-bin-path $install_path \
            install \
            --flag Snowdrift:-dev \
            --pedantic
        hdr "Packing executables"
        find ${install_path} -type f -executable | xargs upx
        hdr "Tarballing"
        rm -rf website/static/tmp/*
        # This forces regeneration of the client session key, which will reset
        # everybody's sessions. This is a bug, but it's better than the current
        # behavior of using whatever key is on my system. :|
        # See https://tree.taiga.io/project/snowdrift/issue/401
        rm -f website/config/client_session_key.aes
        tar czf ${opt_appname}.keter -C website ${contents[@]}
    else
        hdr "Not building, as requested"
    fi
    if $opt_deploy
    then
        hdr "Deploying"
        scp ${opt_appname}.keter `sd-main-dns`:/opt/keter/incoming
    else
        hdr "Not deploying, as requested"
    fi
}

time main
