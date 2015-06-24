# Windows Setup for Snowdrift

Install 32 bit PostgreSQL from
[http://www.enterprisedb.com/products-services-training/pgdownload#windows](http://www.enterprisedb.com/products-services-training/pgdownload#windows)

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Install 32 bit (i386) 7.8.4 version of MinGHC from
[https://github.com/fpco/minghc](https://github.com/fpco/minghc)

The tools have to be 32 bit because on 64 bit
yesod-markdown > hslua fails to build with the following error:
 
    ghc.exe: D:\Sources\snowdrift\.cabal-sandbox\x86_64-windows-ghc-7.8.4\hslua-0.3.13\HShslua-0.3.13.o: unknown symbol `mingw_getsp'

Run the following commands to update cabal packages and install yesod-bin:

    cabal update
    cabal install yesod-bin

Git clone snowdrift source code and follow `Setting up the development database manually`
section in [GUIDE.md](GUIDE.md) (Use `psql -U postgres` to enter the psql
prompt)

Run following commands inside snowdrift folder:

    cabal sandbox init
    cabal install -fdev --enable-tests

Follow `Running the site` section in [GUIDE.md](GUIDE.md)