
## odbcinst.ini on Ubuntu
```
[PostgreSQL]
Description = PostgreSQL ODBC driver
Driver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so
FileUsage = 1

[SQLite]
Description=SQLite ODBC Driver
Driver=/usr/lib/x86_64-linux-gnu/odbc/libsqlite3odbc.so
UsageCount=1
```

## odbcinst.ini on Centos 8
```
[PostgreSQL]
Description = PostgreSQL ODBC driver
Driver = /usr/lib64/psqlodbcw.so
FileUsage = 1
```
