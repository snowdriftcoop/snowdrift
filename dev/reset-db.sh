#!/bin/bash

if [ ! -f "README.md" ]; then
  echo "Please run from the root directory."
  exit 1;
fi

source ./dev/utils.sh

requireWriteOn "/tmp"

dropDatabaseIfExists snowdrift_development
dropTemplateDatabaseIfExists snowdrift_test_template

sudo -u postgres createdb snowdrift_development
sudo -u postgres createdb snowdrift_test_template

psqlCommand "ALTER USER snowdrift_development WITH ENCRYPTED PASSWORD 'snowdrift';"
psqlCommand "GRANT ALL PRIVILEGES ON DATABASE snowdrift_development TO snowdrift_development;"

psqlCommand "ALTER USER snowdrift_test WITH ENCRYPTED PASSWORD 'snowdrift';"
psqlCommand "UPDATE pg_database SET datistemplate = true WHERE datname = 'snowdrift_test_template';"

cp ./dev/devDB.sql /tmp/devDB.sql
cp ./dev/testDB.sql /tmp/testDB.sql
sudo -u postgres psql snowdrift_development < /tmp/devDB.sql
sudo -u postgres psql snowdrift_test_template < /tmp/testDB.sql
rm /tmp/devDB.sql
rm /tmp/testDB.sql

echo ""
echo "All done! If you feel so inclined, change your snowdrift_development user"
echo "password (default: 'snowdrift') by running:"
echo ""
echo "    $ ./dev/change-pw.sh my-new-password"
echo ""
echo "Happy hacking!"
