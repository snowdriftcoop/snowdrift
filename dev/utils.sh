#!/bin/bash

function require {
  hash $1 &>/dev/null || { echo >&2 "required but not found: $1"; exit 1; }
}

function requireWriteOn {
  if [ ! -w $1 ]; then
    echo "Cannot write to $1. Aborting."
    exit 1;
  fi
}

function psqlCommand {
  echo $1 | sudo -u postgres psql
}

function databaseExists {
  sudo -u postgres psql -lqt | cut -d \| -f 1 | grep -w $1 &>/dev/null
}

function dropDatabase {
  psqlCommand "DROP DATABASE $1;"
}

function dropDatabaseIfExists {
  if databaseExists $1; then
    dropDatabase $1
  fi
}

function dropTemplateDatabaseIfExists {
  if databaseExists $1; then
    psqlCommand "UPDATE pg_database SET datistemplate = 'false' WHERE datname = '$1';"
    dropDatabase $1
  fi
}
