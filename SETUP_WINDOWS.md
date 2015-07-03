# Windows Setup for Snowdrift

Install 32 bit PostgreSQL from
[http://www.enterprisedb.com/products-services-training/pgdownload#windows](http://www.enterprisedb.com/products-services-training/pgdownload#windows)

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Git clone snowdrift source code and follow `Setting up the development database manually`
section in [GUIDE.md](GUIDE.md) (Use `psql -U postgres` to enter the psql
prompt)

Follow `Running the site` section in [GUIDE.md](GUIDE.md)
