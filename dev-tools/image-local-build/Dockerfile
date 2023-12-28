FROM centos:7

ARG USER
ARG UID

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Fixes some weird bug with yum taking forever to install things. Doesn't help
# future RUNs, but at least it helps in interactive sessions.
RUN echo "ulimit -n 1024000" > /etc/profile.d/ulimit.sh

# Setup to get postgresql12
RUN rpm --import https://download.postgresql.org/pub/repos/yum/keys/RPM-GPG-KEY-PGDG

# GHCUP's recommended packages, some of which we don't use
# Plus git, which we DO use
RUN ulimit -n 1024000; yum -y install gcc gcc-c++ gmp gmp-devel make ncurses xz perl git zlib-devel openssl-devel

# Prep for postgresql
RUN ulimit -n 1024000; yum install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-7-x86_64/pgdg-redhat-repo-latest.noarch.rpm

# adds repos for clang-7 toolchain, needed for postgresql12-devel
RUN ulimit -n 1024000; yum install -y epel-release centos-release-scl-rh


# The client package has all the tools, server has pg_ctl, and devel is for
# building libs.
RUN ulimit -n 1024000; yum install -y postgresql12 postgresql12-server postgresql12-devel

# Add *all* the postgres tools to the PATH, since e.g. pg_isready is missing
# otherwise.
RUN echo 'export PATH=$PATH:/usr/pgsql-12/bin' > /etc/profile.d/extra-postgres-bin.sh

RUN adduser -u $UID $USER
USER $USER

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_GHC_VERSION=8.0.2 BOOSTRAP_HASKELL_NONINTERACTIVE=1 sh

ENV USER=$USER
WORKDIR /home/$USER/snowdrift
