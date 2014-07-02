#!/bin/sh

if [ ! -f "README.md" ]; then
  echo "Please run from the root directory."
  exit 1;
fi

source ./dev/utils.sh

if [[ "$#" -ne 1 ]]; then
  echo "Usage: $0 my-new-password"
  exit 1
fi

psqlCommand "ALTER USER snowdrift_development WITH ENCRYPTED PASSWORD '$1';"
psqlCommand "ALTER USER snowdrift_test WITH ENCRYPTED PASSWORD '$1';"

echo "All done! Please modify config/postgresql.yml with your new password."
